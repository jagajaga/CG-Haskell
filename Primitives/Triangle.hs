module Primitives.Triangle where

import Graphics.Gloss.Data.Point

type Triangle = (Point, Point, Point)
        
makeTriangle :: Point -> Point -> Point -> Triangle
makeTriangle a b c = (a, b, c)
        
