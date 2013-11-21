module Algorithms.ConvexHull.Andrew (convexHullAndrew) where

import           Algorithms.Rotate
import           Data.List                  (sort)
import           Graphics.Gloss.Data.Vector

convexHullAndrew :: [Vector] -> [Vector]
convexHullAndrew points = case points of 
    [] -> []
    _ ->  upper ++ lower
    where
        sorted = sort points
        upper = chain sorted
        lower = chain (reverse sorted)

chain :: [Vector] -> [Vector]
chain = go []
    where
        go :: [Vector] -> [Vector] -> [Vector]
        go acc@(x:s:xs) (y:ys) = if clockwise s x y
                                 then go (s:xs) (y:ys)
                                 else go (y:acc) ys
        go acc (x:xs) = go (x:acc) xs
        go acc [] = reverse $ tail acc
