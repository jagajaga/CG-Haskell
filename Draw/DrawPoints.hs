module Draw.DrawPoints (drawPoints) where

import           Control.Lens
import           Graphics.Gloss
import           State.State

drawPoints :: State -> Picture
drawPoints = drawPoints' . flip (^.) points

drawPoints' :: [Vector] -> Picture
drawPoints' = Pictures . map (\(x,y) -> Color green $ Translate x y $ Circle 3)
