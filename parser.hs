-- Parser based on RFGrammar

import System.IO

import Control.Applicative((<*))
import Control.Monad (void, liftM)
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Char
import SQLParser.PGSqlParser
import ANormalAST

def :: LanguageDef st
def = emptyDef{ commentStart = "/*"
	, commentEnd = "*/"
	, identStart = letter <|> char '_'
	, identLetter = alphaNum <|> char '_'
	, opStart = oneOf "!&><=:|-+.{}()"
	, opLetter = oneOf "&=|"
	, reservedOpNames = ["!", "&&", ">", "<", ">=", "<=", "==", "-", "+", 
                       ":=", "||", "{", "}", "(", ")"]
	, reservedNames = ["true", "false", "if", "then", "else", "end", 
      "SQL", "assert", "do", "def", "transaction"]
	}


TokenParser
  { identifier = m_identifier
	, reservedOp = m_reservedOp
	, reserved = m_reserved
	, semiSep = m_semiSep
	, semiSep1 = m_semiSep1
  , commaSep = m_commaSep
	, commaSep1 = m_commaSep1
	, stringLiteral = m_stringLiteral
	, whiteSpace = m_whiteSpace 
  , integer = m_integer
  , parens = m_parens{- Parser a -> Parser a -}
  , braces = m_braces} = makeTokenParser def

varParser :: Parser Var_t
varParser = liftM{-( a->b)->Ma->Mb -} Var_T m_identifier

fieldParser :: Parser Field_t
fieldParser = liftM{-( a->b)->Ma->Mb -} Field_T m_identifier

boolParser :: Parser Bool
boolParser =  (m_reserved "true" >> return True)
          <|> (m_reserved "false" >> return False)

fieldAssignmentParser :: Parser (Field_t,ValExprAtom_t)
fieldAssignmentParser = do
  fld <- fieldParser
  _ <- m_reservedOp "="
  valExpAtom <- valExprAtomParser
  return (fld,valExpAtom)


recordParser :: Parser (Record_t ValExprAtom_t)
recordParser = liftM Record_T (m_braces $ m_commaSep1 fieldAssignmentParser)
  
  

valExprAtomParser :: Parser ValExprAtom_t
valExprAtomParser =  try (m_integer >>= \x -> return $ ConstInt $ fromInteger x)
                 <|> try (boolParser >>= \x -> return $ ConstBool x)
                 <|> try (varParser >>= \x -> return $ Var x)
                 <?> "a ValExprAtom"

valExprParser :: Parser ValExpr_t
valExprParser =  try (valExprAtomParser >>= \x -> return $ Atom x)
             <|> try (recordParser >>= \x -> return $ Record x)
             <?> "a value expression" 

unOpParser :: Parser Prim_t
unOpParser = m_reservedOp "!" >> return Not

binOpParser :: Parser Prim_t
binOpParser =  try (m_reservedOp "&&" >> return And)
           <|> try (m_reservedOp "||" >> return Or)
           <|> try (m_reservedOp "==" >> return Equals)
           <|> try (m_reservedOp ">" >> return Gt)
           <|> try (m_reservedOp "<" >> return Lt)
           <|> try (m_reservedOp ">=" >> return Geq)
           <|> try (m_reservedOp "<=" >> return Leq)
           <|> try (m_reservedOp "-" >> return Minus)
           <|> try (m_reservedOp "+" >> return Plus)
           <?> "-- this msg should never be displayed --" 

primParser :: Parser Prim_t
primParser =  try (m_reservedOp "&&" >> return And)
           <|> try (m_reservedOp "||" >> return Or)
           <|> try (m_reservedOp "!" >> return Not)
           <|> try (m_reservedOp "==" >> return Equals)
           <|> try (m_reservedOp ">" >> return Gt)
           <|> try (m_reservedOp "<" >> return Lt)
           <|> try (m_reservedOp ">=" >> return Geq)
           <|> try (m_reservedOp "<=" >> return Leq)
           <|> try (m_reservedOp "-" >> return Minus)
           <|> try (m_reservedOp "+" >> return Plus)
           <?> "-- this msg should never be displayed --" 

primAppParser :: Parser Expr_t
primAppParser =  try (do { p <- unOpParser
                         ; exp <- valExprParser 
                         ; return $ PrimApp p [exp]})
             <|> try (do { exp1 <- valExprParser
                         ; p <- binOpParser
                         ; exp2 <- valExprParser 
                         ; return $ PrimApp p [exp1, exp2]})

formalArgsParser :: Parser [Var_t]
formalArgsParser = m_parens $ m_commaSep varParser

lambdaParser :: Parser Lambda_t
lambdaParser = do
  _ <- m_reservedOp "do"
  args <- formalArgsParser
  body <- stmtParser
  _ <- m_reservedOp "end"
  return $ mkLambda (args,body)

actualArgsParser :: Parser [ValExpr_t]
actualArgsParser = m_parens $ m_commaSep valExprParser

sqlParser :: Parser SQLStatement
sqlParser = (m_reserved "SQL" >> m_stringLiteral >>= 
                \x -> return $ parseSql x)

exprParser :: Parser Expr_t
exprParser =  try (liftM ValExpr valExprParser )
          <|> try (do { 
                      ; x <- valExprAtomParser
                      ; y <- actualArgsParser
                      ; return $ App x y})
          {- <|> try (primAppParser) -}
          <|> try (do { 
                      ; p <- primParser
                      ; y <- actualArgsParser
                      ; return $ PrimApp p y})
          <|> try (liftM Lambda lambdaParser)
          <|> try (liftM SQL sqlParser)
          <?> "an expression" 

txnStmtParser :: Parser Stmt_t
txnStmtParser = do
  m_reserved "transaction"
  m_reserved "do"
  stmt <- stmtParser
  m_reserved "end"
  return stmt

iteStmtParser :: Parser Stmt_t
iteStmtParser = do {m_reserved "if"
		; cond <- m_parens exprParser
		; m_reserved "then"
		; ts <- stmtParser
		; m_reserved "else"
		; fs <- stmtParser
		; m_reserved "end"
		; return $ mkITE (cond,ts,fs)
		}

assmtStmtParser :: Parser Stmt_t
assmtStmtParser = do
  var <- varParser
  m_reservedOp ":="
  expr <- exprParser
  return (var := expr)

assertStmtParser :: Parser Stmt_t
assertStmtParser = m_reserved "assert" >> 
                    liftM Assert (m_parens exprParser)

stmtSeqParser :: Parser [Stmt_t]
stmtSeqParser = m_semiSep1 stmtParser


stmtParser :: Parser Stmt_t
stmtParser =  try txnStmtParser
          {-<|> try (liftM Expr exprParser) -}
          <|> try (assmtStmtParser)
          <|> try (iteStmtParser)
          <|> try (assertStmtParser)
          <|> try (liftM Seq stmtSeqParser)
          <?> "a statement"
