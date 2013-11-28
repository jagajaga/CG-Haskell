module Graphics.CG.Draw.Points (drawPoints ) where

import           Control.Lens
import           Graphics.Gloss

drawPoints :: [Vector] -> Picture
drawPoints = Pictures . map (\(x,y) -> Color green $ Translate x y $ Circle 3)
