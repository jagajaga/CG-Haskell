module Graphics.CG.Primitives.Triangle(Triangle, makeTriangle, sortTrianglePoints) where

import           Data.List                 (sort)
import           Graphics.Gloss.Data.Point

type Triangle = (Point, Point, Point)

makeTriangle :: Point -> Point -> Point -> Triangle
makeTriangle a b c = (a, b, c)

sortTrianglePoints :: Triangle -> Triangle
sortTrianglePoints (a, b, c) = (\[a, b, c] -> (a, b, c)) $ sort [a, b, c]

