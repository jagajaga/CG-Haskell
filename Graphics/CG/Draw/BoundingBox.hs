module Graphics.CG.Draw.BoundingBox(drawBoundingBox) where

import           Control.Lens
import           Graphics.CG.Draw.Lines
import           Graphics.CG.Draw.Points
import           Graphics.CG.Primitives.BoundBox
import           Graphics.CG.State.State
import           Graphics.Gloss
import Graphics.CG.Algorithms.Rotate

drawBoundingBox :: State -> Picture
drawBoundingBox state = Pictures (drawPoints pts : [drawClosedLines (boundingBoxPoints)])
    where
        pts = state ^.points
        bb = boundPoints $ state^.points
        boundingBoxPoints = if (length $ state^.points) > 3 then obb $ state^.points else []

drawBB :: Num t => BoundBox t -> [(t, t)]
drawBB (BoundBox xMin' yMin' xMax' yMax') = [p2, p1, p3, p4] where
        [xMin, yMin] = [xMin' - 1, yMin' - 1]
        [xMax, yMax] = [xMax' + 1, yMax' + 1]
        p1 = (xMin, yMin)
        p2 = (xMin, yMax)
        p3 = (xMax, yMin)
        p4 = (xMax, yMax)
