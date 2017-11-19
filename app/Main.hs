module Main where

  import System.Environment
  import Control.Arrow ((>>>))

  import Parse (parseIsabelleFile)
  import PrettyProlog (isabelleToProlog, prettyProlog)
  import Pretty (docToString)

  translate :: String -> IO ()
  translate fileName = parseIsabelleFile fileName
    >>= (isabelleToProlog
    >>> prettyProlog 
    >>> docToString
    >>> putStr)

  main :: IO ()
  main = do
    fileNames <- getArgs
    mapM_ translate fileNames
