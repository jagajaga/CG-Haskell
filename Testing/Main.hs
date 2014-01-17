module Main where

import           Graphics.CG.Draw.BoundingBox
import           Graphics.CG.Handle.Points
import           Graphics.CG.State.State
import           Graphics.CG.State.State

import           Graphics.Gloss



initialState :: State
initialState = State {
   _points = []
}

drawState :: State -> Picture
drawState = drawBoundingBox

updateState :: Float -> State -> State
updateState _ = id

handleInput = handleInputPoints

main :: IO ()
main = play display black fps initialState drawState handleInput updateState
    where
    fps = 60
    display = InWindow "CG test" (width,height) (0,0)
    width  = 640
    height = 480

