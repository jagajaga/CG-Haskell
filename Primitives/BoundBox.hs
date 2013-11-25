module Primitives.BoundBox(BoundBox (..), boundPoints) where

import           Graphics.Gloss.Data.Point

data BoundBox a = BoundBox !a !a !a !a

boundPoints :: Ord a => [(a, a)] -> BoundBox a
boundPoints ps =
  let
    xs = map fst ps
    ys = map snd ps
  in BoundBox (minimum xs) (minimum ys) (maximum xs) (maximum ys)
