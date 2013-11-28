module Graphics.CG.Handle.Points (handleInputPoints) where

import           Control.Lens
import           Debug.Trace
import           Graphics.CG.State.State
import           Graphics.Gloss.Interface.Pure.Game

handleInputPoints :: Event -> State -> State
handleInputPoints event state
        {-| EventMotion pt@(x, y) <- event-}
        {-= state & points .~ (_init .~ (state^.points) $ [pt])        -}

        | EventKey (MouseButton LeftButton) Down _ pt@(x,y) <- event
        = state & points .~ (_init .~ (state^.points) $ [pt])

        | EventKey (SpecialKey KeySpace) Down _ _ <- event
        = if not $ null $ state ^. points then trace (show $ state^.points) state else state

        | EventKey (Char 'r') Down _ _ <- event
        = state & points .~ []

        | otherwise
        = state
