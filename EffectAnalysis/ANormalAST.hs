module ANormalAST where

  import SQLParser.PGSqlParser

  data Var_t = Var_T String deriving (Eq,Show,Ord)

  data Field_t = Field_T String deriving Show

  data Record_t a = Record_T [(Field_t,a)] deriving Show

  data ValExpr_t = ConstInt Int
                 | ConstBool Bool
                 | DotExp Var_t Field_t
                 | Var Var_t deriving Show

  data Prim_t = Not | And | Or | Geq | Leq | Minus | Plus 
              | Lt | Gt | Equals | Neq deriving Show

  data Type_t = TInt | TString | TBool deriving Show

  data Lambda_t = Lambda_T { lArgs :: [Var_t]
                           , lBody :: Stmt_t} deriving Show

  data Expr_t 	= ValExpr ValExpr_t
                | App ValExpr_t [ValExpr_t]
                | PrimApp Prim_t [ValExpr_t]
                | Lambda Lambda_t
                | Record (Record_t ValExpr_t) 
                | SQL SQLStatement deriving Show

  data Stmt_t = Transaction Stmt_t
              | Expr Expr_t 
              | Var_t := Expr_t
              | ITE Expr_t Stmt_t Stmt_t 
              | Assert (Expr_t)
              | Seq [Stmt_t] deriving Show

  data Method_t 	= Method_T { mName :: String
                             , mArgs :: [Var_t]
                             , mBody :: Stmt_t} deriving Show

  data Program_t = Program_T [Method_t] deriving Show

  -- Builder Functions
 
  mkVar :: String -> Var_t
  mkVar = Var_T

  mkField :: String -> Field_t
  mkField = Field_T

  fieldOfVar :: Var_t -> Field_t
  fieldOfVar (Var_T v) = Field_T v

  mkLambda :: ([Var_t],Stmt_t) -> Lambda_t
  mkLambda (args,body) = Lambda_T {lArgs = args, lBody = body}

  mkMethod :: (String, [Var_t], Stmt_t) -> Method_t
  mkMethod (v,args,stmt) = Method_T {mName = v, mArgs = args, mBody=stmt}

  mkITE :: (Expr_t, Stmt_t, Stmt_t) -> Stmt_t
  mkITE (boolExp,ts,fs) = ITE boolExp ts fs