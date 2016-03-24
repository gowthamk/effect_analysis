module Main where
  import System.IO
  import System.Environment (getArgs)
  -- import Parser (parseString)
  import Exp

  main :: IO ()
  main = do
    {-[fname] <- getArgs
    fhandle <- openFile fname ReadMode  
    contents <- hGetContents fhandle  
    print $ parseString fname contents
    hClose fhandle-}
    putStrLn "-- InitContext ---"
    ctx <- initContext
    putStrLn $ show ctx
