module Main where
  import System.IO
  -- import Prelude hiding (readFile)
  -- import Data.ByteString (readFile)
  import System.Environment (getArgs)
  import Parser (parseString)
  import ANormalAST
  import ArelOfSQL
  import Exp as TC
  import Control.Monad.Trans.State.Lazy (runStateT)

  doParse :: String -> IO Program_t
  doParse fname = do
    fhandle <- openFile fname ReadMode  
    contents <- hGetContents fhandle
    {- Evaluate contents before closing file -}
    contents `seq` hClose fhandle
    let program = case parseString fname contents of
                  Left err -> error $ show err
                  Right p -> p
    --putStrLn $ show program
    return program

  doTypeCheck :: Program_t -> IO TC.Context
  doTypeCheck prog = do
    initCtx <- initContext
    (_,finalCtx) <- runStateT (tcProgram prog) initCtx
    putStrLn $ show finalCtx
    return finalCtx

  main :: IO ()
  main = do
    [fname] <- getArgs
    doParse fname
    return ()
    {-putStrLn "-- InitContext ---"
    ctx <- initContext
    putStrLn $ show ctx-}
