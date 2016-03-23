module SpecLang.RefinementType where

  import Data.IORef
  import System.IO.Unsafe ( unsafePerformIO )
  import qualified ANormalAST as A
  import qualified SpecLang.TypeRefinement as TR

  type Var = A.Var_t
  type TyD = A.Type_t
  type Phi = TR.Predicate

  data Type = Base (Var, TyD, Phi)
            | Arrow ([(Var,TyD)], Type) deriving (Show)

  symbase = "v_"

  counter :: IORef Int
  {-# NOINLINE counter #-}
  counter = unsafePerformIO (newIORef 0)

  genVar :: IO Var
  genVar = do
    count <- readIORef counter
    modifyIORef counter (+1)
    return $ A.mkVar $ symbase++(show count)


  fromTyD :: TyD -> IO Type
  fromTyD t = do
    v <- genVar
    return $ Base (v,t,TR.Truee)
