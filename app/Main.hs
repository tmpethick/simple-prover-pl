module Main where

  import System.Environment
  import Control.Arrow ((>>>))

  import Parse (parseIsabelleFile)
  import PrettyProlog (isabelleToProlog, prettyProlog)
  import Pretty (docToString)
  import TreeOutput (drawTerm)

  translate :: String -> IO ()
  translate fileName = parseIsabelleFile fileName
    >>= (isabelleToProlog
    >>> prettyProlog
    >>> docToString
    >>> putStr)

  main :: IO ()
  main = do
    fileNames <- getArgs
    drawTerm (head fileNames)
    -- mapM_ translate fileNames
