module Main where
  import System.IO
  import System.Environment (getArgs)
  import Parser (parseString)

  main :: IO ()
  main = do
    [fname] <- getArgs
    fhandle <- openFile fname ReadMode  
    contents <- hGetContents fhandle  
    print $ parseString fname contents
    hClose fhandle
