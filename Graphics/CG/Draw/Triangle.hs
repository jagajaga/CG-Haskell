module Graphics.CG.Draw.Triangle where

import           Graphics.CG.Draw.Lines
import           Graphics.CG.Primitives.Triangle
import           Graphics.Gloss

drawTriangle :: Triangle -> Picture
drawTriangle (a, b, c) = drawClosedLines [a, b, c]
