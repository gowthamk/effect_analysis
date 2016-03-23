module SpecLang.RelPredicate where

  import qualified SpecLang.Arel as R
  import qualified ANormalAST as A

  type Var = A.Var_t

  data Relation = R_ String deriving Show
  
  data RelExpr = Arel R.Relation
               | Srel Relation Var deriving Show

  data Predicate = Eq RelExpr RelExpr deriving Show
