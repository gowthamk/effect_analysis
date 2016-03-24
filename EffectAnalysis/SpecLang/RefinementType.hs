module SpecLang.RefinementType where

  import Data.IORef
  import System.IO.Unsafe ( unsafePerformIO )
  import qualified SpecLang.BasePredicate as BP
  import qualified ANormalAST as A
  import qualified SpecLang.TypeRefinement as TypRef

  type Var = A.Var_t
  type TyD = A.Type_t
  type Phi = TypRef.Predicate

  data Type = Base (Var, TyD, Phi)
            | Arrow ([(Var,Type)], Type) deriving (Show,Eq)

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
  fromTyD (A.TArrow (argTyDs,retTyD)) = do
    argBinds <- mapM (\argTyD -> do { v <- genVar
                                    ; argRefTy <- fromTyD argTyD
                                    ; return (v,argRefTy)}) argTyDs
    retRefTy <- fromTyD retTyD
    return $ Arrow (argBinds,retRefTy)
  fromTyD t = do
    v <- genVar
    return $ Base (v,t,TypRef.Truee)

  subst :: [(Var, A.ValExpr_t)] -> Type -> Type
  {- Assumption : bv ∉ dom(substs) -}
  subst substs (Base (bv,tyd,phi)) = Base (bv,tyd, TypRef.subst substs phi)
  subst substs (Arrow (fArgBinds, fResTy)) = Arrow (fArgBinds',fResTy')
    where fArgBinds' = map (\(arg,argTy) -> (arg, subst substs argTy)) fArgBinds
          fResTy' = subst substs fResTy

  mkPrimTy :: (A.Type_t, A.Expr_t) -> IO Type
  mkPrimTy (tyd,exp) = do
    bv <- genVar
    let bvExp = A.ValExpr $ A.Var bv
        phi = TypRef.Base $ BP.Eq bvExp exp
    return $ Base (bv,tyd,phi)

  fromPrimApp :: A.Expr_t -> IO Type
  fromPrimApp e@(A.PrimApp op _) | (op `elem` [ A.Not, A.And, A.Or
                                              , A.Equals, A.Leq, A.Geq
                                              , A.Lt, A.Gt, A.Neq]) = mkPrimTy (A.TBool,e)
  fromPrimApp e@(A.PrimApp op _) | (op `elem` [ A.Minus, A.Plus]) = mkPrimTy (A.TInt,e)
  fromPrimApp e = error $ "RefTy.fromPrimApp called on "++(show e)

  dummy :: IO Type
  dummy = genVar >>= \v -> return $ Base (v,A.TUnknown,TypRef.Truee)
