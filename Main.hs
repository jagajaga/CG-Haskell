module Main where

import           Draw.TriangulationDelauney
import           Handle.HandleInputDelaunay
import           State.DelaunayState

import           Graphics.Gloss

import Data.HashMap.Strict (empty)


initialState :: State
initialState = State {
   _points = [],
   _triangulation = empty
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

