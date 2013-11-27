module Handle.HandleInputDelaunay (handleInputDelaunay) where

import           Algorithms.Triangulation.Delaunay
import           Control.Lens
import           Debug.Trace
import           Graphics.Gloss.Interface.Pure.Game
import           State.DelaunayState

handleInputDelaunay :: Event -> State -> State
handleInputDelaunay event state
        {-| EventMotion pt@(x, y) <- event-}
        {-= state & points .~ (_init .~ (state^.points) $ [pt])        -}

        | EventKey (MouseButton LeftButton) Down _ pt@(x,y) <- event
        = let addPointToState = points .~ (_init .~ (state^.points) $ [pt]) 
              newTriangulation = addPoint (state ^. triangulation) pt in 
            case traceShow (length $ state^.points) (length $ state^.points) of
            x | x < 2 -> state & addPointToState
            2 -> (state & addPointToState) & (triangulation .~ (fst $ traceShow(doTriangulation $ state ^. points) (doTriangulation $ state^.points)))
            otherwise -> traceShow (state & points .~ (_init .~ (state^.points) $ [pt]) & triangulation .~ newTriangulation) (state & points .~ (_init .~ (state^.points) $ [pt]) & triangulation .~ newTriangulatio)


        | EventKey (SpecialKey KeySpace) Down _ _ <- event
        = traceShow (state^.triangulation) state

        | EventKey (Char 'r') Down _ _ <- event
        = state & points .~ []

        | otherwise
        = state
