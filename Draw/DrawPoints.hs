module Draw.DrawPoints (drawPoints, handleInputPoints) where

import           Control.Lens
import           Debug.Trace
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import           State.State

drawPoints :: State -> Picture
drawPoints = drawPoints' . flip (^.) points

drawPoints' :: [Vector] -> Picture
drawPoints' = Pictures . map (\(x,y) -> Color green $ Translate x y $ Circle 3)

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
