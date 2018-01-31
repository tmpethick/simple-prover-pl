module TreeOutput where
-- Outputting tree to PostScript

import Parse
import TreeDrawing
import Data.List.Split
import Data.Maybe
import System.IO
import qualified Data.Map as Map
import ConvertTerm

treeToString :: Tree String -> String
treeToString tree = show tree

-- Simple example of a tree
testTree = Node "a" $ [Node "b" [], Node "c" [Node "e" [], Node "f" []], Node "d" [Node "g" [], Node "h" []]]

drawTermAsTree :: Term -> IO ()
drawTermAsTree term = drawTree designTree
  where designTree = convertTerm term

drawTerm :: String -> IO ()
drawTerm filename = parseIsabelleFile filename
  >>= drawTermAsTree

splitString :: String -> [String]
splitString = splitOn " "

-- Compute depth
type Depth = Int
type MaxMap = Map.Map Depth Int

depthTree :: Tree (String, b) -> Tree Int
depthTree tree = head $ depthTree' [tree]
  where
    mapper (Node (l, _) ns) = Node ((length . splitString) l) (depthTree' ns)
    depthTree' :: [Tree (String, b)] -> [Tree Int]
    depthTree' tree = map mapper tree

-- let rec fold f acc = function
--   | Node (label, ts) ->
--     let acc' = f acc (Node (label, ts))
--     let acc'' = Seq.fold (fold f) acc' ts
--     acc''

-- Alternative name: LayerwiseFold
depthawareFold :: ((a, b) -> Tree c -> (a, b)) -> (a, b) -> Tree c -> (a, b)
depthawareFold f (acc, d) (Node label ts) =
    let (acc', d') = f (acc, d) $ Node label ts
        (acc'', _) = foldl (depthawareFold f) (acc', d') ts
     in (acc'', d)

layerWiseMax :: Tree Int -> (MaxMap, Depth)
layerWiseMax = depthawareFold folder (Map.empty, 0)
  where
    folder :: (MaxMap, Depth) -> Tree Int -> (MaxMap, Depth)
    folder (maxMap, d) (Node label _) =
      let addToMap maxMap (Just a) = if label > d then Map.insert d label maxMap else maxMap
          addToMap maxMap Nothing  = Map.insert d label maxMap
       in (addToMap maxMap $ Map.lookup d maxMap, d + 1)

-- Helper functions
rmoveto :: Float -> Float -> String
rmoveto x y = show (100.0 * x) ++ " " ++ show (-y) ++ " rmoveto"
rlineto :: Float -> Float -> String
rlineto x y = show (100.0 * x) ++ " " ++ show (-y) ++ " rlineto"

getPos :: Tree (a, Float) -> Float
getPos (Node (_, pos) _) = pos

-- Drawing functions
drawChild :: MaxMap -> Depth -> Tree (String, Float) -> [String]
drawChild depthMap depth t = rlineto 0.0 20.0 : (drawNode depthMap (depth + 1) t) ++ [rmoveto 0.0 (-20.0)]

nextChild :: Float -> [String]
nextChild x = [rmoveto x 0.0]

drawNode :: MaxMap -> Depth -> Tree (String, Float) -> [String]
drawNode depthMap depth (Node (l, _) nodes) =
  let lines = splitString l
      spacing = let d = fromJust (Map.lookup depth depthMap)
                 in d - (length lines)
      label = map (\x -> "0 -20 rmoveto (" ++ x ++ ") dup stringwidth pop 2 div neg dup 0 rmoveto exch show 0 rmoveto") lines
   in label ++ [rmoveto 0.0 8.0]
        ++ (if spacing > 0 && not (null nodes) then replicate spacing (rlineto 0.0 20.0) else [])
        ++ drawing nodes
        ++ (if spacing > 0 && not (null nodes) then replicate spacing (rmoveto 0.0 (-20.0)) else [])
        ++ [rmoveto 0.0 (-8.0 + (-20.0) * ((fromIntegral . length) label))]
  where
    drawing :: [Tree (String, Float)] -> [String]
    drawing [] = []
    drawing _  = let positions = map getPos nodes
                     pos = head positions
                     diffs = map (abs . uncurry (-)) $ zip positions (tail positions)
                     children = drawChild depthMap depth (head nodes) ++ concat (map (\(x, t) -> nextChild x ++ drawChild depthMap depth t) (zip diffs (tail nodes)))
                  in rlineto 0.0 10.0 : [rmoveto (-pos) 0.0, rlineto (2.0 * pos) 0.0]
                      ++ children ++ [rmoveto pos (-10.0)]

drawTree :: Tree String -> IO ()
drawTree tree =
  let filename   = "post.ps"
      designTree = design tree
      depthMap   = layerWiseMax $ depthTree designTree
      left       = computeBound min [designTree]
      right      = computeBound max [designTree]
      width      = 65 * (right - left)
      height     = 65 * (depth designTree)
   in do
    out <- openFile filename WriteMode
    hPutStrLn out $ "%!PS"
    hPutStrLn out $ "% Compile to PDF with:"
    hPutStrLn out $ "% gs -sDEVICE=pdfwrite -o output.pdf " ++ filename
    hPutStrLn out $ "<</PageSize[" ++ show (2.0 * width) ++ " " ++ show height ++ "]/ImagingBBox null>> setpagedevice"
    hPutStrLn out $ "/Courier"
    hPutStrLn out $ "20 selectfont"
    hPutStrLn out $ show width ++ " " ++ show height ++ " moveto"
    mapM_ (\v -> hPutStrLn out v) $ drawNode (fst depthMap) 0 designTree
    hPutStrLn out $ "stroke"
    hPutStrLn out $ "showpage"
    hClose out

computeBound :: (Float -> Float -> Float) -> [Tree (String, Float)] -> Float
computeBound f []                            = 0.0
computeBound f (Node (_, v) []       : rest) = f v (computeBound f rest)
computeBound f (Node (_, v) children : rest) = f (v + computeBound f children) (computeBound f rest)

depth :: Tree a -> Depth
depth tree = depth' 0 [tree]
  where
    depth' :: Depth -> [Tree a] -> Depth
    depth' d  []              = d
    depth' d (Node _ ns : rs) = max (depth' (d + 1) ns) (depth' d rs)