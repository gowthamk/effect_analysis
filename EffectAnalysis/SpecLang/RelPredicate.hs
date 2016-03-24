module SpecLang.RelPredicate where

  import qualified SpecLang.Arel as R
  import qualified ANormalAST as A

  type Var = A.Var_t

  data Relation = R_ String deriving (Show,Eq)
  
  data RelExpr = Arel R.Relation
               | Srel Relation Var deriving (Show,Eq)

  data Predicate = Eq RelExpr RelExpr deriving (Show,Eq)

  substInRelExpr :: [(Var,A.ValExpr_t)] -> RelExpr -> RelExpr
  substInRelExpr substs (Arel rel) = Arel $ R.subst substs rel
  substInRelExpr substs (Srel srel v) = Srel srel $ doSubst v
    where doSubst v = case lookup v substs of
                        Nothing -> v
                        Just (A.Var v') -> v'
                        _ -> error "StrucRel applied over non-variable"

  subst :: [(Var,A.ValExpr_t)] -> Predicate -> Predicate
  subst substs (Eq re1 re2) = Eq (f re1) (f re2)
    where f = substInRelExpr substs
