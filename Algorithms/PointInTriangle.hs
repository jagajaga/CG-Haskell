module Algorithms.PointInTriangle(pointInTriangle) where

import           Algorithms.Rotate
import           Graphics.Gloss.Data.Point
import           Primitives.Triangle

pointInTriangle :: Point -> Triangle -> Bool
pointInTriangle p (p1, p2, p3) = not or1 && not or2 && not or3
    where
        or1 = clockwise p p1 p2
        or2 = clockwise p p2 p3
        or3 = clockwise p p3 p1
