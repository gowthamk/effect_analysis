-- Parser based on RFGrammar

module Parser (
  parseString
) where

  -- Parsec rule: once a branch accepts a token then alternative
  -- branches are pruned. No backtracking by default. If needed,
  -- backtracking needs to be added explicitly via "try" combinator.
  import System.IO

  import Control.Applicative((<*))
  import Control.Monad (void, liftM)
  import Control.Exception (throw)
  import Text.Parsec
  import Text.Parsec.String
  import Text.Parsec.Expr
  import Text.Parsec.Token
  import Text.Parsec.Language
  import Text.Parsec.Char
  import SQLParser.PGSqlParser
  import ANormalAST
  import Debug.Trace(trace)

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
    { identifier      = m_identifier
    , reservedOp      = m_reservedOp
    , reserved        = m_reserved
    , semi            = m_semi
    , semiSep         = m_semiSep
    , semiSep1        = m_semiSep1
    , commaSep        = m_commaSep
    , commaSep1       = m_commaSep1
    , stringLiteral   = m_stringLiteral
    , whiteSpace      = m_whiteSpace 
    , integer         = m_integer
    , parens          = m_parens{- Parser a -> Parser a -}
    , braces          = m_braces} = makeTokenParser def

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
    
    
  -- No need to "try" any of the rules of valExpAtom, because the
  -- first token consumed by each rule is unique.
  valExprAtomParser :: Parser ValExprAtom_t
  valExprAtomParser =  (m_integer >>= \x -> return $ ConstInt $ fromInteger x)
                   <|> (boolParser >>= \x -> return $ ConstBool x)
                   <|> (varParser >>= \x -> return $ Var x)
                   <?> "a ValExprAtom"

  valExprParser :: Parser ValExpr_t
  valExprParser =  (valExprAtomParser >>= \x -> return $ Atom x)
               <|> (recordParser >>= \x -> return $ Record x)
               <?> "a value expression" 

  -- There is one-to-one correspondence between reservedOp tokens and
  -- primitive operators. No "try" needed.
  unOpParser :: Parser Prim_t
  unOpParser = m_reservedOp "!" >> return Not

  binOpParser :: Parser Prim_t
  binOpParser =  (m_reservedOp "&&" >> return And)
             <|> (m_reservedOp "||" >> return Or)
             <|> (m_reservedOp "==" >> return Equals)
             <|> (m_reservedOp ">" >> return Gt)
             <|> (m_reservedOp "<" >> return Lt)
             <|> (m_reservedOp ">=" >> return Geq)
             <|> (m_reservedOp "<=" >> return Leq)
             <|> (m_reservedOp "!=" >> return Neq)
             <|> (m_reservedOp "-" >> return Minus)
             <|> (m_reservedOp "+" >> return Plus)
             <?> "a binary operator" 

  primParser :: Parser Prim_t
  primParser =   (m_reservedOp "&&" >> return And)
             <|> (m_reservedOp "||" >> return Or)
             <|> (m_reservedOp "!" >> return Not)
             <|> (m_reservedOp "==" >> return Equals)
             <|> (m_reservedOp ">" >> return Gt)
             <|> (m_reservedOp "<" >> return Lt)
             <|> (m_reservedOp ">=" >> return Geq)
             <|> (m_reservedOp "<=" >> return Leq)
             <|> (m_reservedOp "!=" >> return Neq)
             <|> (m_reservedOp "-" >> return Minus)
             <|> (m_reservedOp "+" >> return Plus)
             <?> "a primitive operator" 

  primAppParser :: Parser Expr_t
  primAppParser =  do { p <- unOpParser
                           ; exp <- valExprParser 
                           ; return $ PrimApp p [exp]}
               <|> do { exp1 <- valExprParser
                           ; p <- binOpParser
                           ; exp2 <- valExprParser 
                           ; return $ PrimApp p [exp1, exp2]}

  formalArgsParser :: Parser [Var_t]
  formalArgsParser = m_parens $ m_commaSep varParser

  lambdaParser :: Parser Lambda_t
  lambdaParser = do
    _ <- m_reservedOp "do"
    args <- formalArgsParser
    body <- seqStmtParser
    _ <- m_reservedOp "end"
    return $ mkLambda (args,body)

  actualArgsParser :: Parser [ValExpr_t]
  actualArgsParser = m_parens $ m_commaSep valExprParser

  sqlParser :: Parser SQLStatement
  sqlParser = (m_reserved "SQL" >> m_stringLiteral >>= 
                  \x -> return $ parseSql x)

  exprParser :: Parser Expr_t
  exprParser =  {-do { 
                   ; p <- primParser
                   ; y <- actualArgsParser
                   ; return $ PrimApp p y} -}
                liftM Lambda lambdaParser
            <|> liftM SQL sqlParser
                -- When multiple rules consume same prefix tokens, use
                -- "try". TODO: Change rules to push try inside.
            <|> try (do { 
                         ; x <- valExprAtomParser
                         ; y <- actualArgsParser
                         ; return $ App x y})
            <|> try (primAppParser)
            <|> liftM ValExpr valExprParser
            <?> "an expression" 

  txnStmtParser :: Parser Stmt_t
  txnStmtParser = do
    m_reserved "transaction"
    m_reserved "do"
    stmt <- seqStmtParser
    m_reserved "end"
    return stmt

  iteStmtParser :: Parser Stmt_t
  iteStmtParser = do {m_reserved "if"
      ; cond <- m_parens exprParser
      ; m_reserved "then"
      ; ts <- seqStmtParser
      ; m_reserved "else"
      ; fs <- seqStmtParser
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

  -- It is a good practice to put the discriminative rules (rules that
  -- begin by eating a reserved token) ahead of non-discriminative
  -- ones.
  stmtParser :: Parser Stmt_t
  stmtParser =  txnStmtParser
            <|> iteStmtParser
            <|> assertStmtParser
            <|> assmtStmtParser
            <?> "a statement"

  stmtSeqParser :: Parser [Stmt_t]
  stmtSeqParser = do
    -- Note: This doesn't handle the case when stmtParser succeeds,
    -- but m_semi fails (i.e., we forgot a semi-colon). 
    -- TODO: Fixme
    fstStmt <- stmtParser
    m_semi
    restStmts <- try stmtSeqParser <|> return []
    return $ fstStmt:restStmts

  seqStmtParser :: Parser Stmt_t
  seqStmtParser = liftM Seq $ stmtSeqParser

  methodParser :: Parser Method_t
  methodParser = do
    m_reserved "def"
    name <- m_identifier
    args <- formalArgsParser
    body <- seqStmtParser
    m_reserved "end"
    return $ mkMethod (name,args,body)

  methodSeqParser :: Parser [Method_t]
  methodSeqParser = do
    fstMeth <- methodParser
    restMeths <- (try methodSeqParser) <|> (eof >> return [])
    return $ fstMeth:restMeths

  programParser :: Parser Program_t
  programParser = liftM Program_T methodSeqParser

  parseString :: String -> Either ParseError Program_t
  parseString s = parse programParser "a program" s


