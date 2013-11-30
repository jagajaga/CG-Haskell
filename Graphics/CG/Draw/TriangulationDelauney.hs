module Graphics.CG.Draw.TriangulationDelauney where

import           Control.Lens
import           Graphics.CG.Algorithms.Triangulation.Delaunay
import           Graphics.CG.Draw.Points
import           Graphics.CG.Draw.Triangle
import           Graphics.CG.Primitives.BoundBox
import           Graphics.CG.Primitives.Triangle
import           Graphics.CG.State.DelaunayState
import           Graphics.Gloss

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
