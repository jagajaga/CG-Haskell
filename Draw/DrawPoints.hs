module Draw.DrawPoints (drawPoints, drawPointsFromState) where

import           Control.Lens
import           Graphics.Gloss
import           State.State

drawPointsFromState :: State -> Picture
drawPointsFromState = drawPoints . flip (^.) points

drawPoints :: [Vector] -> Picture
drawPoints = Pictures . map (\(x,y) -> Color green $ Translate x y $ Circle 3)
