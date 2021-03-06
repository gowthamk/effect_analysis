module SpecLang.RelPredicate where

  import qualified SpecLang.Arel as R
  import qualified ANormalAST as A

  type Var = A.Var_t

  data Relation = R_ String deriving (Eq)

  instance Show Relation where
    show (R_ name) = "R"++name

  type StrucRel = Relation
  
  data RelExpr = ARel R.Relation
               | SRel (Relation,Var) deriving (Eq)

  instance Show RelExpr where
    show (ARel rel) = show rel
    show (SRel (rel,v)) = (show rel)++"("++(A.varToString v)++")"

  data Predicate = Eq RelExpr RelExpr deriving (Show,Eq)

  substInRelExpr :: [(Var,A.ValExpr_t)] -> RelExpr -> RelExpr
  substInRelExpr substs (ARel rel) = ARel $ R.subst substs rel
  substInRelExpr substs (SRel (srel,v)) = SRel (srel, doSubst v)
    where doSubst v = case lookup v substs of
                        Nothing -> v
                        Just (A.Var v') -> v'
                        _ -> error "StrucRel applied over non-variable"

  subst :: [(Var,A.ValExpr_t)] -> Predicate -> Predicate
  subst substs (Eq re1 re2) = Eq (f re1) (f re2)
    where f = substInRelExpr substs

  mkSRelARelEq :: StrucRel -> R.Relation -> Var -> Predicate
  mkSRelARelEq srel arel v = (SRel (srel,v)) `Eq` (ARel arel)
