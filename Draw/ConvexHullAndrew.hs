module Draw.ConvexHullAndrew(drawConvexHullAndrew) where

import           Algorithms.ConvexHull.Andrew
import           Control.Lens
import           Draw.Lines
import           Draw.Points(drawPoints)
import           Graphics.Gloss
import           State.State

drawConvexHullAndrew :: State -> Picture
drawConvexHullAndrew state = Pictures (drawPointsFromState state : [drawClosedLinesFromState (state & (points .~ convexHullPoints))])
    where
        convexHullPoints = convexHullAndrew $ state^.points
