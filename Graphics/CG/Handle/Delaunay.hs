module Graphics.CG.Handle.Delaunay (handleInputDelaunay) where

import           Control.Lens
import           Debug.Trace
import           Graphics.CG.Algorithms.Triangulation.Delaunay
import           Graphics.CG.Primitives.BoundBox
import           Graphics.CG.State.DelaunayState
import           Graphics.Gloss.Interface.Pure.Game

handleInputDelaunay :: Event -> State -> State
handleInputDelaunay event state
        | EventKey (MouseButton LeftButton) Down _ pt@(x,y) <- event
        = let addPointToState = points .~ (_init .~ (state^.points) $ [pt])
              (trig, triangles, bb) = doTriangulation $ state^.points ++ [pt]
              (newTriangulation, newBB) = if pointInBoundBox pt $ state^.boundBox then ((addPoint (state ^. triangulation) pt), state^.boundBox) else (trig, bb) in
            case length $ state^.points of
            x | x < 2 -> state & addPointToState
            2 -> state & addPointToState & triangulation .~ trig & boundBox .~ bb
            otherwise -> state & points .~ (_init .~ (state^.points) $ [pt]) & triangulation .~ newTriangulation & boundBox .~ newBB


        | EventKey (SpecialKey KeySpace) Down _ _ <- event
        = if not $ null $ state^.points then trace (("Triangles: ") ++ (show $ triangulationToTriangles $ state^.triangulation) ++ ("\r\n\r\n") ++ ("Points: ") ++ (show $ state^.points) ++ ("\r\n\r\n")) state else state

        | EventKey (Char 'r') Down _ _ <- event
        = state & points .~ []

        | otherwise
        = state
