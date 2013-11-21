module Main where

import           Draw.DrawConvexHullAndrew
import           Draw.DrawPoints
import           State.State

import           Graphics.Gloss

initialState :: State
initialState = State {
   _points = []
   ---TODO press state
}

drawState :: State -> Picture
drawState = drawConvexHullAndrew

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

