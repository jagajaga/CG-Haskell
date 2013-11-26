module Draw.TriangulationDelauney where

import           Algorithms.Triangulation.Delaunay
import           Control.Lens
import           Draw.Points
import           Draw.Triangle
import           Draw.Circule
import           Graphics.Gloss
import           Primitives.Triangle
import           State.DelaunayState

import Debug.Trace
import Data.List

drawTrianulationDelauney :: State -> Picture
drawTrianulationDelauney state = Pictures $ drawPoints pts : map drawTriangle trig {-++ (map drawCirculeFromTriangle trig)-}
    where
        pts = state^.points
        trig = trace ("pts: " ++ (show $ state^.points) ++ " triangles: " ++ (show $ length $ triangles)) (triangles)
        (a, triangles) = case length pts of 
            x | x <= 3 -> doTriangulation $ state^.points 
            otherwise -> (state^.triangulation, triangulationToTriangles $ state^.triangulation)

---TODO draw normal visualisation
