module Draw.TriangulationDelauney where

import           Algorithms.Triangulation.Delaunay
import           Control.Lens
import           Draw.Circule
import           Draw.Points
import           Draw.Triangle
import           Graphics.Gloss
import           Primitives.BoundBox
import           Primitives.Triangle
import           State.DelaunayState

import           Data.List
import           Debug.Trace

drawTrianulationDelauney :: State -> Picture
drawTrianulationDelauney state = Pictures $ drawPoints pts : map drawTriangle triangles
    where
        pts = state^.points
        (a, triangles, b) = case length pts of
            x | x <= 3 -> doTriangulation $ pts
            otherwise -> (state^.triangulation, triangulationToTriangles $ state^.triangulation, state ^. boundBox)

---TODO draw normal visualisation
