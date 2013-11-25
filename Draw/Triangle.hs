module Draw.Triangle where

import Graphics.Gloss
import Primitives.Triangle
import Draw.DrawLines

drawTriangle :: Triangle -> Picture
drawTriangle (a, b, c) = drawClosedLines [a, b, c]
