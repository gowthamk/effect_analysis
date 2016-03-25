module ArelOfSQL where

  import Database.HsSqlPpp.Syntax
  -- import Database.HsSqlPpp.Internals.AstInternal
  import qualified SpecLang.Arel as R
  import qualified ANormalAST as A
  import Data.List(intercalate)
  import Data.List.Split(splitOn)
  import Debug.Trace(trace)
  import Control.Exception.Base(assert)

  {-
   - Some assumptions about SQL syntax:
   - 1. If a table name is qualified, then all the fields of that
   - table used in the query are also qualified.
   - 2. Except JOIN, all queries act on a single table.
   -}

  unimpl :: String -> a
  unimpl s = error $ "Unimpl. "++s

  unimplShow :: Show a => a -> b
  unimplShow x = unimpl $ show x 


  doItTableRef :: TableRef -> (R.Relation, Maybe R.Var)
  doItTableRef (Tref _ name) = (R.R_ $ doItName name, Nothing)
  doItTableRef (TableAlias _ nmc tableRef) = 
    case doItTableRef tableRef of 
      (rel, Nothing) -> (rel, Just $ A.mkVar $ doItNameComponent nmc)
      x -> unimpl "Nested aliasing not supported"
  doItTableRef x = unimplShow x

  doItTrefList :: [TableRef] -> (R.Relation, Maybe R.Var)
  doItTrefList [] = error "No tables!"
  doItTrefList [tableRef] = doItTableRef tableRef
  doItTrefList x = unimpl $ "doItTrefList "++(show x)

  doItNameComponent :: NameComponent -> String
  doItNameComponent (Nmc s) = s
  doItNameComponent (QNmc s) = s
  doItNameComponent (AntiNameComponent s) = s

  doItName :: Name -> String
  doItName (AntiName str) = str
  doItName (Name _ nmcs) = dotString
    where dotString = intercalate "." (map doItNameComponent nmcs)

  doItSExpForId :: ScalarExpr -> String
  doItSExpForId (Identifier _ name) = doItName name
  doItSExpForId (Parens _ se) = doItSExpForId se
  doItSExpForId x = unimplShow x

  doItSExpForValExpr :: ScalarExpr -> A.ValExpr_t
  doItSExpForValExpr (BooleanLit _ b) = A.ConstBool b
  doItSExpForValExpr (NumberLit _ nStr) = A.ConstInt $ read nStr
  {- Currently treating all strings as program variables. 
   - Full generality Unimpl. -}
  doItSExpForValExpr (StringLit _ str) = A.Var $ A.mkVar str
  doItSExpForValExpr se@(Identifier _ _) = 
    case splitOn "." $ doItSExpForId se of 
      [v] -> A.Var $ A.mkVar v
      [v,f] -> A.DotExp (A.mkVar v) (A.mkField f)
      x -> unimplShow x
  doItSExpForValExpr (Parens _ se) = doItSExpForValExpr se
  doItSExpForValExpr x = unimplShow x

  multiOpOfId :: String -> A.ValExpr_t -> [A.ValExpr_t] -> R.Predicate
  multiOpOfId "idin"  = R.In  
  multiOpOfId x = unimplShow x
 
  {- doItSExpForPred -}
  doItSExpForPred :: ScalarExpr -> R.Predicate
  doItSExpForPred (BooleanLit _ True) = R.Truee 
  doItSExpForPred (BooleanLit _ False) = R.Falsee
  doItSExpForPred (BinaryOp _ name se1 se2) | (doItName name) == "=" = 
      R.Eq (doItSExpForValExpr se1) (doItSExpForValExpr se2)
  doItSExpForPred (BinaryOp _ name se1 se2) | ((doItName name) == "and") = 
      R.And (doItSExpForPred se1) (doItSExpForPred se2)
  doItSExpForPred (BinaryOp _ name se1 se2) | ((doItName name) == "or") = 
      R.Or (doItSExpForPred se1) (doItSExpForPred se2)
  {-doItSExpForPred (SpecialOp _ name (opSExp:argSExps)) =
    case doItName name of
      "arraysub" -> let multiOp = multiOpOfId $ doItSExpForId opSExp
                        args = map doItSExpForValExpr argSExps
                    in multiOp args-}
  doItSExpForPred (InPredicate _ se True (InList _ seList)) = 
    let lhsValExp = doItSExpForValExpr se
        rhsValExps = map doItSExpForValExpr seList
    in R.In lhsValExp rhsValExps
  doItSExpForPred (InPredicate _ se True (InQueryExpr _ qe)) = 
    let lhsValExp = doItSExpForValExpr se
        rhsRel = doItQuery qe
    in R.InRel lhsValExp rhsRel
  doItSExpForPred (InPredicate ann se False inList) = 
    R.Not $ doItSExpForPred (InPredicate ann se True inList)
  doItSExpForPred (Parens _ se) = doItSExpForPred se
  doItSExpForPred se = unimplShow se

  doItSelectItem :: SelectItem -> A.Field_t
  doItSelectItem (SelExp _ se) = A.Field_T $ doItSExpForId se
  doItSelectItem (SelectItem _ se {- AS -}_) = A.Field_T $ doItSExpForId se

  -- Returns empty list if SELECT *
  doItSelectItemList :: [SelectItem] -> [A.Field_t]
  doItSelectItemList [SelExp _ (Star _)] = []
  doItSelectItemList [SelExp _ (QStar _ _)] = []
  doItSelectItemList siList = map doItSelectItem siList

  doItMaybeBoolExpr :: MaybeBoolExpr -> R.Predicate
  doItMaybeBoolExpr Nothing = R.Truee
  doItMaybeBoolExpr (Just se) = doItSExpForPred se

  doItQuery :: QueryExpr -> R.Relation
  doItQuery (Select { selSelectList=SelectList _ selItemList 
                    , selWhere=wherePred
                    , selTref=tRefList}) = 
    let (rel,maybeAlias) = doItTrefList tRefList
        freePred = doItMaybeBoolExpr wherePred
        pred = \x -> case maybeAlias of 
            Nothing -> freePred `R.qualifyPredWith` x
            Just y -> [(y,x)] `R.substQualifiedInPred` freePred 
        flds = doItSelectItemList selItemList
    in R.Pi flds $ R.Sigma pred rel
        

  doIt :: Statement -> R.Relation
  doIt (QueryStatement _ q) = doItQuery q
  doIt s = unimpl $ "doIt "++(show s)

  
