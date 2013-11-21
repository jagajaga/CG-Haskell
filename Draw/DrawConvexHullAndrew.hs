module Draw.DrawConvexHullAndrew where

import           Algorithms.ConvexHull.Andrew
import           Control.Lens
import           Draw.DrawLines
import           Draw.DrawPoints(drawPoints)
import           Graphics.Gloss
import           State.State

drawConvexHullAndrew :: State -> Picture
drawConvexHullAndrew state = Pictures ([drawPoints state] ++ [drawClosedLines $ (state & (points .~ convexHullPoints))])
    where
        convexHullPoints = convexHullAndrew $ state^.points
        ---TODO drawLine function
