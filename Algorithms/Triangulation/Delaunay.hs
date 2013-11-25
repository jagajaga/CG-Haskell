module Algorithms.Triangulation.Delaunay  where

import           Algorithms.PointInTriangle
import           Algorithms.Vectors
import           Graphics.Gloss
import           Primitives.BoundBox
import           Primitives.Triangle
import           Primitives.UnorderedPair

import           Data.List

import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HMap
import           Data.HashSet               (HashSet)
import qualified Data.HashSet               as HSet

import           Data.Maybe                 (fromJust)


type Triangulation = HashMap Point (HashSet (UnorderedPair Point))

neighbors :: Triangulation -> Triangle -> [(Point, Triangle)]
neighbors trig (p1',p2',p3') = findNeighbors p1' (p2',p3') ++ findNeighbors p2' (p1',p3') ++ findNeighbors p3' (p1',p2')
  where findNeighbors p1 (p2,p3) = HSet.toList $ HSet.map fromJust $ HSet.delete Nothing $
                                   HSet.map (\pr -> case (other p2 pr, other p3 pr) of
                                                (Just p3'',Nothing) | p3 /= p3'' -> Just (p3,makeTriangle p1 p3'' p2)
                                                (Nothing,Just p2'') | p2 /= p2'' -> Just (p2,makeTriangle p1 p2'' p3)
                                                (_      ,       _)            -> Nothing)
                                   (trig HMap.! p1)

emptyTriangulation :: Triangulation
emptyTriangulation = HMap.empty

insertTriangle :: Triangulation -> Triangle -> Triangulation
insertTriangle neighbours (p1',p2',p3') = newTriangulation
  where newTriangulation = foldl (\nbrs (pt,rst) -> HMap.insertWith HSet.union pt rst nbrs) neighbours
                           [(p1, HSet.singleton $ makeUnorderedPair p2 p3),
                            (p2, HSet.singleton $ makeUnorderedPair p1 p3),
                            (p3, HSet.singleton $ makeUnorderedPair p1 p2)]
        [p1, p2, p3] = sort [p1', p2', p3']

baseTriangulation :: [Point] -> Triangulation
baseTriangulation pts = foldl' insertTriangle emptyTriangulation [makeTriangle p1 p2 p3, makeTriangle p2 p3 p4]
  where (BoundBox xMin' yMin' xMax' yMax') = boundPoints pts
        [xMin, yMin] = [xMin' - 1, yMin' - 1]
        [xMax, yMax] = [xMax' + 1, yMax' + 1]
        p1 = (xMin, yMin)
        p2 = (xMin, yMax)
        p3 = (xMax, yMin)
        p4 = (xMax, yMax)



triangulation :: [Vector] -> [Triangle]
triangulation [] = []
triangulation pts = [(0,0,0)]
