module SpecLang.Effect (
  EffKind(..),
  Effect(..),
  EffSet(..),
  emptySet,
  singletonSet,
  simple,
  mkBindSet,
  setUnion
) where

  import qualified ANormalAST as A
  import qualified SpecLang.Arel as R

  type Var = A.Var_t
  type Relation = R.Relation
  
  data EffKind = Rd | Wr deriving Show

  data Effect = Effect { txnid :: Maybe Int,
                         obj   :: Var,
                         kind  :: EffKind} deriving Show

  data EffSet = Const [Effect]
              | Bind Relation (Var -> EffSet)
              | Union EffSet EffSet

  instance Show EffSet where
    show (Const effs) = "{"++(show effs)++"}"
    show (Bind rel effSetFn) = "{ "++(show $ effSetFn $ A.mkVar "x")
                                ++"| x ∈ "++(show rel)++"}"
    show (Union effSet1 effSet2) = (show effSet1)++" ∪ "++(show effSet2)

  emptySet :: EffSet
  emptySet = Const []

  singletonSet :: Effect -> EffSet
  singletonSet e = Const [e]

  simple :: (Var,EffKind) -> Effect
  simple (v,k) = Effect {txnid = Nothing, obj = v, kind = k}

  mkBindSet :: Relation -> (Var -> EffSet) -> EffSet
  mkBindSet = Bind

  setUnion :: (EffSet,EffSet) -> EffSet
  setUnion (s1,s2) = Union s1 s2
