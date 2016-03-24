module SpecLang.BasePredicate where

  import qualified ANormalAST as A

  type Var = A.Var_t

  data Predicate = Eq A.Expr_t A.Expr_t
                 | Iff A.Expr_t A.Expr_t deriving (Show)

  {- Don't sweat the small stuff. ToDo: Derive it. -}
  subst :: [(Var,A.ValExpr_t)] -> Predicate -> Predicate
  subst substs pred = case pred of 
      (Eq e1 e2) -> Eq (f e1) (f e2)
      (Iff e1 e2) -> Iff (f e1) (f e2)
    where f = A.substInExpr substs

