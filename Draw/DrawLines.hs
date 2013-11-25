module Draw.DrawLines (drawPath, drawClosedLines, drawPathFromState, drawClosedLinesFromState) where

import           Control.Lens
import           Graphics.Gloss
import           State.State

myHead :: [a] -> [a]
myHead [] = []
myHead (x:_) = [x]

drawPathFromState :: State -> Picture
drawPathFromState = drawPath . flip (^.) points

drawPath :: [Vector] -> Picture
drawPath = Color white . Line

drawClosedLinesFromState :: State -> Picture
drawClosedLinesFromState = drawClosedLines . flip (^.) points

drawClosedLines :: [Vector] -> Picture
drawClosedLines s = Color white . Line $ ss
    where ss = s ++ myHead s
