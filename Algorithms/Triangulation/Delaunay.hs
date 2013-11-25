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

emptyTriangulation :: Triangulation
emptyTriangulation = HMap.empty

makeDelaunay :: Triangulation -> [Triangle] -> Triangulation
makeDelaunay trig [] = trig
makeDelaunay triangulation (t:ts) =
  case makeDelaunay' triangulation (neighbors triangulation t) of
    Just (trig',insertTriangles,deleteTriangles) -> makeDelaunay trig' (foldr (:) (foldl' (flip delete) ts deleteTriangles) insertTriangles)
    Nothing -> makeDelaunay triangulation ts
    where
      makeDelaunay' :: Triangulation -> [(Point,Triangle)] -> Maybe (Triangulation,[Triangle],[Triangle])
      makeDelaunay' trig neighbours = case filter (\(p3,(p1',p3',p2')) -> shouldFlip (p1', p2') p3' p3) neighbours of
        [] -> Nothing
        ((p3,(p1',p3',p2')):_) -> Just (flipEdge trig (p1', p2') p3' p3)

shouldFlip :: (Point, Point) -> Point -> Point -> Bool
shouldFlip (p1, p2) p3 p3' = angle p1 p3 p2 + angle p1 p3' p2 > pi &&
                             angle p3 p1 p3' + angle p3 p2 p3' <= pi

flipEdge :: Triangulation -> (Point,Point) -> Point -> Point -> (Triangulation,[Triangle],[Triangle])
flipEdge trig (a,b) c c' = (foldl' insertTriangle (foldl' deleteTriangle trig deleteTriangles) insertTriangles, insertTriangles, deleteTriangles)
  where insertTriangles = [makeTriangle c a c',makeTriangle c b c']
        deleteTriangles = [makeTriangle a b c, makeTriangle a b c']

addPoint :: Triangulation -> Point -> Triangulation
addPoint trig pt = makeDelaunay trig' splittedTriangles
  where
    potentialTriangles = triangulationToTriangles trig
    tris = filter (pointInTriangle pt) potentialTriangles
    tri = head tris
    (trig',(t1,t2,t3)) = splitTriangle trig tri pt
    splittedTriangles = [t1,t2,t3]

addPoints :: Triangulation -> [Point] -> Triangulation
addPoints trig [] = trig
addPoints trig pts = foldl addPoint trig pts

neighbors :: Triangulation -> Triangle -> [(Point, Triangle)]
neighbors trig (p1',p2',p3') = findNeighbors p1' (p2',p3') ++ findNeighbors p2' (p1',p3') ++ findNeighbors p3' (p1',p2')
  where findNeighbors p1 (p2,p3) = HSet.toList $ HSet.map fromJust $ HSet.delete Nothing $
                                   HSet.map (\pr -> case (other p2 pr, other p3 pr) of
                                                (Just p3'', Nothing) | p3 /= p3'' -> Just (p3, makeTriangle p1 p3'' p2)
                                                (Nothing, Just p2'') | p2 /= p2'' -> Just (p2, makeTriangle p1 p2'' p3)
                                                (_, _) -> Nothing) (trig HMap.! p1)

insertTriangle :: Triangulation -> Triangle -> Triangulation
insertTriangle neighbours (p1',p2',p3') = newTriangulation
  where newTriangulation = foldl (\nbrs (pt,rst) -> HMap.insertWith HSet.union pt rst nbrs) neighbours
                           [(p1, HSet.singleton $ makeUnorderedPair p2 p3),
                            (p2, HSet.singleton $ makeUnorderedPair p1 p3),
                            (p3, HSet.singleton $ makeUnorderedPair p1 p2)]
        [p1, p2, p3] = sort [p1', p2', p3']

deleteTriangle :: Triangulation -> Triangle -> Triangulation
deleteTriangle trig (p1,p2,p3) =  HMap.adjust (HSet.delete $ makeUnorderedPair p2 p3) p1 $
                                  HMap.adjust (HSet.delete $ makeUnorderedPair p1 p3) p2 $
                                  HMap.adjust (HSet.delete $ makeUnorderedPair p1 p2) p3 trig

splitTriangle :: Triangulation -> Triangle -> Point -> (Triangulation,(Triangle,Triangle,Triangle))
splitTriangle trig (p1, p2, p3) pt = (trig',(t1,t2,t3))
  where
    trig' = foldl' insertTriangle (deleteTriangle trig (p1,p2,p3)) [t1,t2,t3]
    t1 = makeTriangle p1 p2 pt
    t2 = makeTriangle p1 p3 pt
    t3 = makeTriangle p2 p3 pt

baseTriangulation :: [Point] -> Triangulation
baseTriangulation pts = foldl' insertTriangle emptyTriangulation [makeTriangle p1 p2 p3, makeTriangle p2 p3 p4]
  where (BoundBox xMin' yMin' xMax' yMax') = boundPoints pts
        [xMin, yMin] = [xMin' - 1, yMin' - 1]
        [xMax, yMax] = [xMax' + 1, yMax' + 1] --- hack  pts = [0 7, 24 33, 10 13, 20 0, 22 11]
        p1 = (xMin, yMin)
        p2 = (xMin, yMax)
        p3 = (xMax, yMin)
        p4 = (xMax, yMax)

triangulationToTriangles :: Triangulation -> [Triangle]
triangulationToTriangles trig = concatMap (\(p1,nPts) -> map (\(UnorderedPair p2 p3) -> (p1,p2,p3)) (HSet.toList nPts)) ptsWithNeighbors
  where
    pts = HMap.keys trig
    ptsWithNeighbors = map (\pt -> (pt, trig HMap.! pt)) pts

removeHelperPoints :: [Point] -> Triangulation -> Triangulation
removeHelperPoints pts trig = removeHelperPoints' (HMap.keys trig \\ pts) trig
  where
    removeHelperPoints' [] trig' = trig'
    removeHelperPoints' (p:ps) trig' = case HMap.lookup p trig' of
      Just neighbours -> removeHelperPoints' ps $
                   HMap.delete p $
                   HSet.foldl' (\trig'' (UnorderedPair nbor1 nbor2) -> HMap.adjust (HSet.filter (not . isElem p)) nbor1 $
                                                  HMap.adjust (HSet.filter (not . isElem p)) nbor2 trig'')
                   trig' neighbours
      Nothing -> removeHelperPoints' ps trig'

triangulation :: [Vector] -> [Triangle]
triangulation [] = []
triangulation pts' = case pts of
    (_:_:_:_) -> triangulationToTriangles $ removeHelperPoints pts trig
    _tooFew   -> []
  where trig = addPoints (baseTriangulation pts) pts
        pts  = nub pts'
