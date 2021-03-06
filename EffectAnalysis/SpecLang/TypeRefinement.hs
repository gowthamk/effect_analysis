module SpecLang.TypeRefinement where

  import Data.List (intercalate)
  import qualified ANormalAST as A
  import qualified SpecLang.RelPredicate as RP
  import qualified SpecLang.BasePredicate as BP

  type Var = A.Var_t

  data Predicate = Truee
                 | Falsee
                 | Rel RP.Predicate
                 | Base BP.Predicate
                 | And [Predicate]

  instance Show Predicate where
    show (Truee) = "True"
    show (Falsee) = "False"
    show (Rel rp) = show rp
    show (Base bp) = show bp
    show (And preds) = intercalate " ∧ " (map show preds)

  {- Note: Trivial implementation of Eq. ToDo: Fixme.-}
  instance Eq Predicate where
    Truee == Truee = True
    Falsee == Falsee = True
    _ == _ = False

  subst :: [(Var,A.ValExpr_t)] -> Predicate -> Predicate 
  subst substs (Rel rp) = Rel $ RP.subst substs rp
  subst substs (Base bp) = Base $ BP.subst substs bp
  subst substs (And preds) = And $ map (subst substs) preds
  subst substs Truee = Truee
  subst substs Falsee = Falsee

  fromRP :: RP.Predicate -> Predicate
  fromRP = Rel

  fromBP :: BP.Predicate -> Predicate
  fromBP = Base
