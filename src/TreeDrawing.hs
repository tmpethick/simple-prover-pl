module TreeDrawing where

data Tree a = Node a [Tree a] deriving (Show)

type Extent = [(Float, Float)]

moveTree :: Tree (a, Float) -> Float -> Tree (a, Float)
moveTree (Node (label, x) subtrees) x' = Node (label, x + x') subtrees

moveExtent :: Extent -> Float -> Extent
moveExtent e x = map (\(l, r) -> (l + x, r + x)) e

merge :: Extent -> Extent -> Extent
merge [] qs   = qs
merge ps []   = ps
merge ((p, _) : ps) ((_, q) : qs) = (p, q) : merge ps qs

mergeList :: [Extent] -> Extent
mergeList es = foldr merge [] es

rmax :: Float -> Float -> Float
rmax p q = if p > q then p else q

fit :: Extent -> Extent -> Float
fit ((_, p) : ps) ((q, _) : qs) = rmax (fit ps qs) (p - q + 1.0)
fit _ _                         = 0.0

fitListL :: [Extent] -> [Float]
fitListL es = fitListL' [] es
  where
      fitListL' :: Extent -> [Extent] -> [Float]
      fitListL' _ []          = []
      fitListL' acc (e:es)    =
        let x = fit acc e
         in x : fitListL' (merge acc (moveExtent e x)) es

flipExtent :: Extent -> Extent
flipExtent = map (\(p, q) -> (-q, -p))

fitListR :: [Extent] -> [Float]
fitListR = reverse . map negate . fitListL . map flipExtent . reverse

mean :: Float -> Float -> Float
mean x y = (x + y) / 2.0

fitList :: [Extent] -> [Float]
fitList es = map (uncurry mean) (zip (fitListL es) (fitListR es))

design :: Tree a -> Tree (a, Float)
design tree = fst (design' tree)
  where 
    design' :: Tree a -> (Tree (a, Float), Extent)
    design' (Node label subtrees) =
      let (trees, extents) = unzip (map design' subtrees)
          positions = fitList extents
          ptrees = map (uncurry moveTree) (zip trees positions)
          pextents = map (uncurry moveExtent) (zip extents positions)
          resultextent = (0.0, 0.0) : mergeList pextents
          resulttree = Node (label, 0.0) ptrees
      in (resulttree, resultextent)