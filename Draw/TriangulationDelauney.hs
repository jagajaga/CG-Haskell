module Draw.TriangulationDelauney where

import           Algorithms.Triangulation.Delaunay
import           Control.Lens
import           Draw.Points
import           Draw.Triangle
import           Graphics.Gloss
import           Primitives.Triangle
import           State.State

drawTrianulationDelauney :: State -> Picture
drawTrianulationDelauney state = Pictures $ drawPointsFromState state : map drawTriangle (triangulation $ state^.points)
