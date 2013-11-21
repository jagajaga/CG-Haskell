module Handle.HandleInputPoints (handleInputPoints) where

import           Control.Lens
import           Debug.Trace
import           Graphics.Gloss.Interface.Pure.Game
import           State.State

handleInputPoints :: Event -> State -> State
handleInputPoints event state
        {-| EventMotion pt@(x, y) <- event-}
        {-= state & points .~ (_init .~ (state^.points) $ [pt])        -}

        | EventKey (MouseButton LeftButton) Down _ pt@(x,y) <- event
        = state & points .~ (_init .~ (state^.points) $ [pt])

        | EventKey (SpecialKey KeySpace) Down _ _ <- event
        = trace (show $ state^.points) state

        | EventKey (Char 'r') Down _ _ <- event
        = state & points .~ []

        | otherwise
        = state
