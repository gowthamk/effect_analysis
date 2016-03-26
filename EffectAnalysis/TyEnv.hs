{-# Language DeriveDataTypeable, FlexibleInstances, TypeSynonymInstances #-}
module TyEnv (
  T,
  VarNotFound_t(..),
  empty,
  add,
  remove,
  lookup,
  mem,
  (!)
) where

  import Debug.Trace (trace)
  import Data.List (intercalate)
  import Prelude hiding (lookup)
  import Data.Typeable.Internal
  import Control.Exception
  import qualified Data.Map.Strict as M
  import qualified SpecLang.RefinementType as RefTy
  import qualified ANormalAST as A

  type Type = RefTy.Type
  type Var = A.Var_t
  data T = T (M.Map Var Type)

  instance Show T where
    show (T m) = "\n[\n\t"++(intercalate "\n\t" (map toStr (M.toList m)))++"\n]\n"
      where toStr (k,v) = (show k)++" :-> "++(show v)

  data VarNotFound_t = VarNotFound Var deriving (Show, Typeable)
  instance Exception VarNotFound_t

  empty :: T
  empty = T $ M.empty

  add :: T -> (Var,Type) -> T
  add (T m) (v,t) = T $ M.insert v t m

  mem :: T -> Var -> Bool
  mem (T m) v = M.member v m

  remove :: T -> Var -> T
  remove (T m) v = T $ M.delete v m

  lookup :: T -> Var -> Maybe Type
  lookup (T m) v = M.lookup v m

  (!) :: T -> Var -> Type
  (T m) ! v = case lookup (T m) v of
      Just t -> t
      Nothing -> throw $ VarNotFound v
