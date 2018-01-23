module Main where

  import System.Environment
  import Control.Arrow ((>>>))

  import Parse (parseIsabelleFile)
  import PrettyProlog (isabelleToProlog, prettyProlog)
  import Pretty (docToString)
  import TreeOutput (draw)

  translate :: String -> IO ()
  translate fileName = parseIsabelleFile fileName
    >>= (isabelleToProlog
    >>> prettyProlog 
    >>> docToString
    >>> putStr)

  main :: IO ()
  main = do
    fileNames <- getArgs
    draw (head fileNames)
    -- mapM_ translate fileNames
