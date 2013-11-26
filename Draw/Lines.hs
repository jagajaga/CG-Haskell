module Draw.Lines (drawPath, drawClosedLines) where

import           Control.Lens
import           Graphics.Gloss

myHead :: [a] -> [a]
myHead [] = []
myHead (x:_) = [x]

drawPath :: [Vector] -> Picture
drawPath = Color white . Line

drawClosedLines :: [Vector] -> Picture
drawClosedLines s = Color white . Line $ ss
    where ss = s ++ myHead s
