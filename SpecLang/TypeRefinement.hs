module TypeRefinement where

  import qualified RelPredicate as RP
  import qualified BasePredicate as BP

  data Predicate = Truee
                 | Falsee
                 | Rel RP.Predicate
                 | Base BP.Predicate
                 | And [Predicate] deriving (Show)
