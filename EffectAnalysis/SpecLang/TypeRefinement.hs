module SpecLang.TypeRefinement where

  import qualified SpecLang.RelPredicate as RP
  import qualified SpecLang.BasePredicate as BP

  data Predicate = Truee
                 | Falsee
                 | Rel RP.Predicate
                 | Base BP.Predicate
                 | And [Predicate] deriving (Show)
