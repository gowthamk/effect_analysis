module SQLParser.PGSqlParser (
SQLStatement(..),
parseSql
)where

import System.Environment

import Language.Haskell.Exts
import Data.Generics.Uniplate.Data
import Database.HsSqlPpp.TypeCheck
import Database.HsSqlPpp.Catalog
import Database.HsSqlPpp.Types
import Database.HsSqlPpp.Annotation
import Database.HsSqlPpp.Syntax
import Database.HsSqlPpp.Parse
import Data.Text.Lazy
import Control.Exception

type SQLStatement = Database.HsSqlPpp.Syntax.Statement

showNoAnns :: Show a => a -> String
showNoAnns = p stripA
  where
    stripA :: Exp -> Exp
    stripA = transformBi $ \x ->
               case x of
                 (Paren (RecConstr (UnQual (Ident "Annotation")) _)) ->
                          Con $ UnQual $ Ident "Ann"
                 x1 -> x1
    p f s =
        case parseExp (show s) of
          ParseOk ast -> prettyPrint (f ast)
          x -> error $ show x


parseSql :: String -> SQLStatement
parseSql inp =  case parseStatements defaultParseFlags 
                          "./errors" Nothing (pack inp) of
                  Left err -> error (show err)
                  Right [e] ->  e
                  Right _ -> error "Expected single SQL statement. Got multiple."
             

