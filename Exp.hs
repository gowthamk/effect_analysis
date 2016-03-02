module Exp where

  import Database.HsSqlPpp.Syntax
  -- import Database.HsSqlPpp.Internals.AstInternal
  import qualified SpecLang as S
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


  doItTableRef :: TableRef -> (S.Relation, Maybe S.Var)
  doItTableRef (Tref _ name) = (S.R_ $ doItName name, Nothing)
  doItTableRef (TableAlias _ nmc tableRef) = 
    case doItTableRef tableRef of 
      (rel, Nothing) -> (rel, Just $ A.mkVar $ doItNameComponent nmc)
      x -> unimpl "Nested aliasing not supported"
  doItTableRef x = unimplShow x

  doItTrefList :: [TableRef] -> (S.Relation, Maybe S.Var)
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
  doItSExpForId x = unimplShow x

  doItSExpForValExpr :: ScalarExpr -> A.ValExpr_t
  doItSExpForValExpr (BooleanLit _ b) = A.ConstBool b
  doItSExpForValExpr (NumberLit _ nStr) = A.ConstInt $ read nStr
  doItSExpForValExpr se@(Identifier _ _) = 
    case splitOn "." $ doItSExpForId se of 
      [v] -> A.Var $ A.mkVar v
      [v,f] -> A.DotExp (A.mkVar v) (A.mkField f)
      x -> unimplShow x
  doItSExpForValExpr x = unimplShow x

  binOpOfId :: String -> A.ValExpr_t -> A.ValExpr_t -> S.Predicate
  binOpOfId "=" = S.Eq
  binOpOfId x = unimplShow x

  multiOpOfId :: String -> A.ValExpr_t -> [A.ValExpr_t] -> S.Predicate
  multiOpOfId "idin"  = S.In  
  multiOpOfId x = unimplShow x
 
  doItInListForValExps :: InList -> [A.ValExpr_t]
  doItInListForValExps (InList _ seList) = map doItSExpForValExpr seList

  doItSExpForPred :: ScalarExpr -> S.Predicate
  doItSExpForPred (BooleanLit _ True) = S.Truee 
  doItSExpForPred (BooleanLit _ False) = S.Falsee
  doItSExpForPred (BinaryOp _ name se1 se2) = 
    let binOp = binOpOfId $ doItName name
        arg1 = doItSExpForValExpr se1
        arg2 = doItSExpForValExpr se2
    in binOp arg1 arg2
  {-doItSExpForPred (SpecialOp _ name (opSExp:argSExps)) =
    case doItName name of
      "arraysub" -> let multiOp = multiOpOfId $ doItSExpForId opSExp
                        args = map doItSExpForValExpr argSExps
                    in multiOp args-}
  doItSExpForPred (InPredicate _ se True inList) = 
    let lhsValExp = doItSExpForValExpr se
        rhsValExps = doItInListForValExps inList
    in S.In lhsValExp rhsValExps
  doItSExpForPred (InPredicate ann se False inList) = 
    S.Not $ doItSExpForPred (InPredicate ann se True inList)
  doItSExpForPred se = unimplShow se

  doItSelectItem :: SelectItem -> A.Field_t
  doItSelectItem x@(SelExp _ _) = unimplShow x
  doItSelectItem (SelectItem _ se _) = A.Field_T $ doItSExpForId se

  -- Returns empty list if SELECT *
  doItSelectItemList :: [SelectItem] -> [A.Field_t]
  doItSelectItemList [SelExp _ (Star _)] = []
  doItSelectItemList [SelExp _ (QStar _ _)] = []
  doItSelectItemList siList = map doItSelectItem siList

  doItMaybeBoolExpr :: MaybeBoolExpr -> S.Predicate
  doItMaybeBoolExpr Nothing = S.Truee
  doItMaybeBoolExpr (Just se) = doItSExpForPred se

  doItQuery :: QueryExpr -> S.Relation
  doItQuery (Select { selSelectList=SelectList _ selItemList 
                    , selWhere=wherePred
                    , selTref=tRefList}) = 
    let (rel,maybeAlias) = doItTrefList tRefList
        freePred = doItMaybeBoolExpr wherePred
        pred = \x -> case maybeAlias of 
            Nothing -> freePred `S.qualifyPredWith` x
            Just y -> [(y,x)] `S.substQualifiedInPred` freePred 
        flds = doItSelectItemList selItemList
    in S.Pi flds $ S.Sigma pred rel
        

  doIt :: Statement -> S.Relation
  doIt (QueryStatement _ q) = doItQuery q
  doIt s = unimpl $ "doIt "++(show s)

  
