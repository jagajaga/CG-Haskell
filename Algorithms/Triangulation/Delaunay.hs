module Algorithms.Triangulation.Delaunay (triangulation) where

import Primitives.Triangle
import Primitives.UnorderedPair
import Graphics.Gloss

import Data.List

import Data.HashMap.Strict(HashMap)
import Data.HashSet(HashSet)


type Triangulation = HashMap Point (HashSet (UnorderedPair Point))

triangulate :: [Vector] -> [Triangle]
triangulate [] = []
triangulate pts' = 
  case pts of
    (_:_:_:_) -> map (\(a, b, c) -> makeTriangle a b c) $ triangulationToTris $ removeHelperPoints pts trig
    _tooFew   -> []
  where trig = addPoints (baseTriangulation pts) pts
        pts  = nub pts'
