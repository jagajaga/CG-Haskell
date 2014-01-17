module Graphics.CG.Draw.Points (drawPoints, drawPoint) where

import           Graphics.Gloss

drawPoints :: [Vector] -> Picture
drawPoints = Pictures . map (\(x,y) -> Color green $ Translate x y $ Circle 3)

drawPoint :: Vector -> Picture
drawPoint (x, y) = Color red $ Translate x y $ Circle 3
