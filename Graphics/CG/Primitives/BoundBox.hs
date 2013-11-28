module Graphics.CG.Primitives.BoundBox(BoundBox (..), boundPoints, pointInBoundBox) where

import           Graphics.Gloss.Data.Point

data BoundBox a = BoundBox !a !a !a !a deriving (Show, Eq)

boundPoints :: Ord a => [(a, a)] -> BoundBox a
boundPoints ps =
  let
    xs = map fst ps
    ys = map snd ps
  in BoundBox (minimum xs) (minimum ys) (maximum xs) (maximum ys)

pointInBoundBox :: Point -> BoundBox (Float) -> Bool
pointInBoundBox pt (BoundBox a b c d) = pointInBox pt (a, b) (c, d)
