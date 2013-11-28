module Draw.ConvexHullAndrew(drawConvexHullAndrew) where

import           Algorithms.ConvexHull.Andrew
import           Control.Lens
import           Draw.Lines
import           Draw.Points
import           Graphics.Gloss
import           State.State

drawConvexHullAndrew :: State -> Picture
drawConvexHullAndrew state = Pictures (drawPoints pts : [drawClosedLines (convexHullPoints)])
    where
        pts = state ^.points
        convexHullPoints = convexHullAndrew $ state^.points
