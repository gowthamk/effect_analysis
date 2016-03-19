module Effect (
  EffKind(..),
  Effect(..),
  EffSet(..)
) where

  import qualified ANormalAST as A
  import qualified Arel as R

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

