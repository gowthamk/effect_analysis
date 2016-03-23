{-# Language DeriveDataTypeable #-}
module TyEnv (
  T,
  VarNotFound_t(..),
  empty,
  add,
  remove,
  lookup,
  mem
) where

  import Prelude hiding (lookup)
  import Data.Typeable.Internal
  import Control.Exception
  import qualified Data.Map.Strict as M
  import qualified SpecLang.RefinementType as RefTy
  import qualified ANormalAST as A

  type Type = RefTy.Type
  type Var = A.Var_t
  type T = M.Map Var Type

  data VarNotFound_t = VarNotFound String deriving (Show, Typeable)
  instance Exception VarNotFound_t

  empty :: T
  empty = M.empty

  add :: T -> (Var,Type) -> T
  add m (v,t) = M.insert v t m

  mem :: T -> Var -> Bool
  mem m v = M.member v m

  remove :: T -> Var -> T
  remove m v = M.delete v m

  lookup :: T -> Var -> Maybe Type
  lookup m v = M.lookup v m

  (!) :: T -> Var -> Type
  m ! v = case lookup m v of
      Just t -> t
      Nothing -> throw $ VarNotFound (show v)
