module Graphics.CG.Draw.ConvexHullAndrew(drawConvexHullAndrew) where

import           Control.Lens
import           Graphics.CG.Algorithms.ConvexHull.Andrew
import           Graphics.CG.Draw.Lines
import           Graphics.CG.Draw.Points
import           Graphics.CG.State.State
import           Graphics.Gloss

drawConvexHullAndrew :: State -> Picture
drawConvexHullAndrew state = Pictures (drawPoints pts : [drawClosedLines (convexHullPoints)])
    where
        pts = state ^.points
        convexHullPoints = convexHullAndrew $ state^.points
