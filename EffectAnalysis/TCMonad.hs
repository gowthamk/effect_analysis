{-# Language NamedFieldPuns, RecordWildCards #-}
module TCMonad where

  import Debug.Trace (trace)
  import Control.Exception (evaluate)
  import Control.Monad
  import Control.Monad.Trans.State
  import Control.Monad.Trans (liftIO)
  import Data.Char (toLower, toUpper)
  import Utils (assert)
  import ANormalAST
  import qualified ArelOfSQL
  import qualified TyEnv as VE
  import qualified SpecLang.Arel as R
  import qualified SpecLang.RelPredicate as RP
  import qualified SpecLang.BasePredicate as BP
  import qualified SpecLang.TypeRefinement as TypRef
  import qualified SpecLang.RefinementType as RefTy
  import qualified SpecLang.Effect as E

  type TyD = Type_t

  data Context = Context { varEnv :: VE.T
                         , effSet :: E.EffSet
                         , method :: String
                         , txnid  :: Maybe TxnId_t} deriving Show

  type TC a = StateT Context IO a

  mkCtx :: (VE.T, E.EffSet) -> Context
  mkCtx (ve,effs) = Context {varEnv = ve, effSet = effs, method="", txnid=Nothing}

  rmwVE :: (VE.T -> VE.T) -> TC ()
  rmwVE f = do
    Context {varEnv, effSet, method, txnid} <- get
    let ve' = f varEnv
    put $ Context { varEnv = ve', effSet = effSet
                  , method = method, txnid = txnid} 

  rmwEffSet :: (E.EffSet -> E.EffSet) -> TC ()
  rmwEffSet f =  do
    Context {varEnv, effSet, method, txnid} <- get
    let effSet' = f effSet
    put $ Context { varEnv = varEnv, effSet = effSet'
                  , method = method, txnid = txnid} 

  putMethod :: String -> TC ()
  putMethod mname = do
    Context {varEnv, effSet, txnid, ..} <- get
    put $ Context {varEnv=varEnv, effSet=effSet, txnid=txnid, method=mname}

  getCurMethod :: TC String
  getCurMethod = do
    Context {method, ..} <- get
    return method

  putTxnId :: TxnId_t -> TC ()
  putTxnId newtxnid = do
    Context {varEnv, effSet, txnid, ..} <- get
    put $ Context { varEnv=varEnv, effSet=effSet
                  , txnid= Just newtxnid, method=method}
   
  getCurTxnId :: TC (Maybe TxnId_t)
  getCurTxnId = do
    Context {txnid, ..} <- get
    return txnid

  preludeBinds :: [(String, TyD)]
  preludeBinds = [ ("stringLength", TString --> TInt)
                 , ("userId", TUser --> TInt)
                 , ("userName", TUser --> TString)
                 , ("userAdmin", TUser --> TBool)
                 , ("relId", TRel --> TInt)
                 , ("relFollowerId", TRel --> TInt)
                 , ("relFollowedId", TRel --> TInt)
                 , ("postId", TPost --> TInt)
                 , ("postUserId", TPost --> TInt)]

  initContext :: IO Context
  initContext = do
    prelude <- foldM addToVE VE.empty preludeBinds
    return $ mkCtx (prelude, E.emptySet)
    where
      addToVE ve (name,tyd) = 
        let fn = mkVar name
            predFn = \x -> \v -> 
                TypRef.fromBP $ (exprOfVar v) `BP.Eq` (mkApp1 (fn,x))
        in do
             refTy <- RefTy.arrowFromBinder1 (tyd,predFn)
             return $ VE.add ve (mkVar name,refTy)
   
  tcBegin :: TC ()
  tcBegin = do
    initCtx <- liftIO initContext
    put initCtx

  tcProgram :: Program_t -> TC ()
  tcProgram (Program_T meths) = 
    let doItMeth tcM meth = tcM >> (tcMeth meth)
    in foldl doItMeth tcBegin meths

  tcMeth :: Method_t -> TC ()
  tcMeth (Method_T {mArgs, mBody, mName, ..}) =
    let argTyds = mArgs
        argRefTysIO = mapM (\(arg,tyd) -> 
                            do { refTy <- RefTy.fromTyD tyd
                               ; return (arg,refTy)}) argTyds
    in do 
        argRefTys <- liftIO argRefTysIO
        rmwVE (\ve -> foldl VE.add ve argRefTys)
        putMethod mName
        tcStmt mBody 
        return ()

  tcStmt :: Stmt_t -> TC RefTy.Type
  tcStmt (Transaction stmt) = do
    mname <- getCurMethod
    putTxnId $ txnIdOfMethod mname
    tcStmt stmt
  tcStmt (Expr expr) = tcExpr expr
  tcStmt (v := expr) = do
    refTy <- tcExpr expr
    rmwVE (\ve -> VE.add ve (v,refTy))
    return refTy
  tcStmt (ITE guardE tStmt fStmt) = do
    tcExpr guardE
    tcStmt tStmt
    tcStmt fStmt
  tcStmt (Seq stmts) = do 
    dummyRefTy <- liftIO $ RefTy.dummy
    foldM (\_ -> tcStmt) dummyRefTy stmts
  tcStmt stmt = unimplShow stmt

  tcExpr :: Expr_t -> TC RefTy.Type
  tcExpr (ValExpr vexp) = tcValExpr vexp
  tcExpr (App (DotExp v f) argsvexps) = do
    Context {varEnv,effSet} <- get
    vty <- liftIO $ evaluate $ varEnv VE.! v
    let vtyd = RefTy.toTyD vty
        prefix = lowerFirst $ tail $ show vtyd
        suffix = upperFirst $ (fieldName f)
        newFun = Var $ mkVar $ prefix++suffix
        newArgs = (Var v):argsvexps
        newExp = App newFun newArgs
    tcExpr newExp
  tcExpr (App fvexp argvexps) = do
    fRefTy <- tcValExpr fvexp 
    argTys <- mapM tcValExpr argvexps
    let argBinds = zip argvexps argTys
    return $ tcApp fRefTy argBinds
  tcExpr primApp@(PrimApp _ _ ) = do
    resTy <- liftIO $ RefTy.fromPrimApp primApp
    return resTy
  tcExpr (Lambda lam) = tcLambda lam
  tcExpr (SQL sql) = 
    let arel = ArelOfSQL.doIt sql
        {- Write effects Unimpl. -}
        recTyD = tyDOfRecsIn arel
        listTyD = listTyDOf recTyD
        rmem = rmemOf recTyD
        rmemVar = varOfStrucRel rmem
        rmemTyD = tydOfStrucRel (listTyD, recTyD)
        rpredFn = RP.mkSRelARelEq rmem arel
        typRefFn = \bv -> TypRef.fromRP $ rpredFn bv
    in do rmemRefTy <- liftIO $ RefTy.fromTyD rmemTyD
          rmwVE (\ve -> VE.add ve (rmemVar,rmemRefTy))
          txnId <- getCurTxnId
          let newEffs = E.mkBindSet arel 
                (\v -> E.singletonSet $ E.simple (txnId,v,E.Rd))
          rmwEffSet (\effSet -> E.setUnion (effSet,newEffs))
          {- Return value is a list whose Rmem = Arel of SQL -}
          listRefTy <- liftIO $ RefTy.fromBinder (listTyD,typRefFn)
          return listRefTy
  tcExpr expr = unimplShow expr

  tcLambda :: Lambda_t -> TC RefTy.Type
  tcLambda (Lambda_T {lArgs,lBody,..}) =
    let argTyds = lArgs
        argRefTysIO = mapM (\(arg,tyd) -> 
                            do { refTy <- RefTy.fromTyD tyd
                               ; return (arg,refTy)}) argTyds
    in do 
        argRefTys <- liftIO argRefTysIO
        rmwVE (\ve -> foldl VE.add ve argRefTys)
        tcStmt lBody 

  tcValExpr :: ValExpr_t -> TC RefTy.Type
  tcValExpr e@(ConstBool _) = do
    typ <- liftIO $ RefTy.mkPrimTy (TBool, ValExpr e)
    return typ
  tcValExpr e@(ConstInt _) = do
    typ <- liftIO $ RefTy.mkPrimTy (TInt, ValExpr e)
    return typ
  tcValExpr (Var v) = do
    Context {varEnv,effSet} <- get
    return $ varEnv VE.! v
  tcValExpr e@(DotExp _ _) = error $ "tcValExpr called on DotExp: "++(show e) 

  tcApp :: RefTy.Type{-type of the function -} 
        -> [(ValExpr_t,RefTy.Type)]{- args and types -} 
        -> RefTy.Type{- type of the application -}
  tcApp (RefTy.Arrow (fArgBinds,fResTy)) argBinds = 
    let mkSubst (fArg,fArgRefty) (arg,argRefTy) = 
            let isSub = argRefTy <: fArgRefty
                errMsg = (show argRefTy)++" <: " ++(show fArgRefty)
                x = assert isSub errMsg
            in x `seq` (fArg,arg){-[arg/fArg]-}
        areEqLen = length fArgBinds == length argBinds
        errMsg = "length ("++(show fArgBinds)++") == length("
                          ++(show argBinds)++")"
        x = assert areEqLen errMsg
        substs = x `seq` zipWith mkSubst fArgBinds argBinds
    in RefTy.subst substs fResTy


  varOfStrucRel :: RP.StrucRel -> Var_t
  varOfStrucRel (RP.R_ name) = mkVar $ "R"++name

  tydOfStrucRel :: (TyD{-listTyD-},TyD{-recTyD-}) -> TyD
  tydOfStrucRel (listTyD,recTyD) = TArrow ([listTyD,recTyD],TBool)

  rmemOf :: TyD -> RP.StrucRel
  rmemOf TPost = RP.R_ "memPost"
  rmemOf TUser = RP.R_ "memUser"
  rmemOf TRel = RP.R_ "memRel"

  listTyDOf :: TyD -> TyD
  listTyDOf TPost = TPostList
  listTyDOf TUser = TUserList
  listTyDOf TRel = TRelList

  tyDOfRecsIn :: R.Relation -> TyD
  tyDOfRecsIn (R.R_ "microposts") = TPost
  tyDOfRecsIn (R.R_ "users") = TUser
  tyDOfRecsIn (R.R_ "relationships") = TRel
  tyDOfRecsIn (R.Pi [] rel) = tyDOfRecsIn rel
  tyDOfRecsIn (R.Sigma _ rel) = tyDOfRecsIn rel
  tyDOfRecsIn r = unimpl $ " tyDOfRecsIn "++(show r)

  (<:) :: RefTy.Type -> RefTy.Type -> Bool
  ty1 <: ty2 = ty1 == ty2

  txnIdOfMethod :: String -> TxnId_t
  txnIdOfMethod "getFeed" =  GF
  txnIdOfMethod "deleteUser" =  DU
  txnIdOfMethod "addPost" =  AP
  txnIdOfMethod "deletePost" =  DP
  txnIdOfMethod "addUser" =  AU
  txnIdOfMethod "addFollower" =  AF
  txnIdOfMethod "removeFollower" =  RF
  txnIdOfMethod "" = error $ "Unexpected transaction outside a method"

  lowerFirst :: String -> String
  lowerFirst (x:xs) = (toLower x):xs
  lowerFirst s = s

  upperFirst :: String -> String
  upperFirst (x:xs) = (toUpper x):xs
  upperFirst s = s

  unimpl :: String -> a
  unimpl s = error $ "Unimpl. "++s

  unimplShow :: Show a => a -> b
  unimplShow x = unimpl $ show x 

  ignoreM :: Monad m => m a -> m ()
  ignoreM m = m >> (return ())
