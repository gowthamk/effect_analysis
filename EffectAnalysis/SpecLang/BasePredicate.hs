module SpecLang.BasePredicate where

  import qualified ANormalAST as A

  type Var = A.Var_t

  data Predicate = Eq A.Expr_t A.Expr_t
                 | Iff A.Expr_t A.Expr_t deriving Show

