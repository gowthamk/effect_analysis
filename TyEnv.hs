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
  import qualified ANormalAST as A
  import Control.Exception
  import qualified Data.Map.Strict as M

  type T = M.Map A.Var_t A.Type_t

  data VarNotFound_t = VarNotFound String deriving (Show, Typeable)
  instance Exception VarNotFound_t

  empty :: T
  empty = M.empty

  add :: T -> (A.Var_t,A.Type_t) -> T
  add m (v,t) = M.insert v t m

  mem :: T -> A.Var_t -> Bool
  mem m v = M.member v m

  remove :: T -> A.Var_t -> T
  remove m v = M.delete v m

  lookup :: T -> A.Var_t -> Maybe A.Type_t
  lookup m v = M.lookup v m

  (!) :: T -> A.Var_t -> A.Type_t
  m ! v = case lookup m v of
      Just t -> t
      Nothing -> throw $ VarNotFound (show v)
