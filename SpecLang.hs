module SpecLang where

  import qualified ANormalAST as A

  type Var = A.Var_t
  
  data EffKind = Rd | Wr deriving Show

  data Predicate = Eq A.ValExpr_t A.ValExpr_t
                 | In A.ValExpr_t [A.ValExpr_t]
                 | And Predicate Predicate
                 | Or Predicate Predicate
                 | Not Predicate
                 | Truee
                 | Falsee deriving Show

  data Relation = R_ String
                | Pi [A.Field_t] Relation 
                | Sigma (Var -> Predicate) Relation
                | Join Predicate Relation Relation

  instance Show Relation where
    show (R_ name) = "R_{"++name++"}"
    show (Pi flds rel) = "π_{"++(show flds)++"}("++(show rel)++")"
    show (Sigma predFn rel) = "σ_{\\x."++(show $ predFn $ mkVar "x")
                               ++"}("++(show rel)++")"
    show _ = "Unimpl."

  data Effect = Effect { txnid :: Maybe Int,
                         obj   :: Var,
                         kind  :: EffKind} deriving Show

  data EffSet = Const [Effect]
              | Bind Relation (Var -> EffSet)
              | Union EffSet EffSet

  instance Show EffSet where
    show (Const effs) = "{"++(show effs)++"}"
    show (Bind rel effSetFn) = "{ "++(show $ effSetFn $ mkVar "x")
                                ++"| x ∈ "++(show rel)++"}"
    show (Union effSet1 effSet2) = (show effSet1)++" ∪ "++(show effSet2)

  mkVar :: String -> Var
  mkVar = A.Var_T

  mapValExpsInPred :: (A.ValExpr_t -> A.ValExpr_t) -> Predicate -> Predicate
  mapValExpsInPred f (Eq ve1 ve2) = Eq (f ve1) (f ve2)
  mapValExpsInPred f (In ve ves) = In (f ve) (map f ves)
  mapValExpsInPred f (And p1 p2) = And (mapValExpsInPred f p1) (mapValExpsInPred f p2) 
  mapValExpsInPred f (Or p1 p2) = Or (mapValExpsInPred f p1) (mapValExpsInPred f p2) 
  mapValExpsInPred f (Not p) = Not (mapValExpsInPred f p)
  mapValExpsInPred f p = p


  mapQualifiedInPred :: (Var -> Var) -> Predicate -> Predicate
  mapQualifiedInPred mapf pred = mapValExpsInPred mapg pred
    where mapg (A.DotExp x f) = A.DotExp (mapf x) f 
          mapg ve = ve

  qualifyPredWith :: Predicate -> Var -> Predicate
  qualifyPredWith pred x = mapValExpsInPred doIt pred
    where doIt (A.Var f) = A.DotExp x (A.fieldOfVar f)
          doIt ve = ve

  -- Subst notation [(y,x)] denotes [y->x] map
  substQualifiedInPred :: [(Var,Var)] -> Predicate -> Predicate
  substQualifiedInPred substs pred = mapQualifiedInPred doSubst pred
    where doSubst oldVar = case lookup oldVar substs of 
                              Nothing -> oldVar
                              Just newVar -> newVar

