module Main where

import           Graphics.CG.Draw.TriangulationDelauney
import           Graphics.CG.Handle.Delaunay
import           Graphics.CG.State.DelaunayState
import           Graphics.CG.State.DelaunayState


import           Graphics.Gloss



initialState :: State
initialState = State {
   _points = []
}

drawState :: State -> Picture
drawState = drawTrianulationDelauney

updateState :: Float -> State -> State
updateState _ = id

handleInput = handleInputDelaunay

main :: IO ()
main = play display black fps initialState drawState handleInput updateState
    where
    fps = 60
    display = InWindow "CG test" (width,height) (0,0)
    width  = 640
    height = 480

