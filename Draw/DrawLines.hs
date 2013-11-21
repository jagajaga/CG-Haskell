module Draw.DrawLines (drawPath, drawClosedLines) where

import           Control.Lens
import           Graphics.Gloss
import           State.State

myHead :: [a] -> [a]
myHead [] = []
myHead (x:_) = [x]

drawPath :: State -> Picture
drawPath = drawPath' . flip (^.) points

drawPath' :: [Vector] -> Picture
drawPath' = Color white . Line

drawClosedLines :: State -> Picture
drawClosedLines = drawClosedLines' . flip (^.) points

drawClosedLines' :: [Vector] -> Picture
drawClosedLines' s = Color white . Line $ ss
    where ss = s ++ myHead s
