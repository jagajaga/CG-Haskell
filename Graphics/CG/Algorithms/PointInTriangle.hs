module Graphics.CG.Algorithms.PointInTriangle(pointInTriangle) where

import           Graphics.CG.Algorithms.Rotate
import           Graphics.CG.Primitives.Triangle
import           Graphics.Gloss.Data.Point

pointInTriangle :: Point -> Triangle -> Bool
pointInTriangle p (p1, p2, p3) = (c1 >= 0 && c2 >= 0 && c3 >= 0) || (c1 <= 0 && c2 <= 0 && c3 <= 0)
    where
        c1 = fromEnum $ orientation p p1 p2
        c2 = fromEnum $ orientation p p2 p3
        c3 = fromEnum $ orientation p p3 p1
