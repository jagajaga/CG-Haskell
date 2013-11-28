module Graphics.CG.Algorithms.Triangulation.Delaunay  where

import           Graphics.CG.Algorithms.PointInTriangle
import           Graphics.CG.Algorithms.Vectors
import           Graphics.CG.Primitives.BoundBox
import           Graphics.CG.Primitives.Triangle
import           Graphics.CG.Primitives.Triangulation
import           Graphics.CG.Primitives.UnorderedPair
import           Graphics.Gloss

import           Data.List

import           Data.HashMap.Strict                    (HashMap)
import qualified Data.HashMap.Strict                    as HMap
import           Data.HashSet                           (HashSet)
import qualified Data.HashSet                           as HSet

import           Data.Maybe                             (fromJust)


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
addPoint trig pt
  | null tris = error "addPoint: no tris"
  | otherwise = makeDelaunay trig' splittedTriangles
  where
    potentialTriangles = nub $ map sortTrianglePoints $ triangulationToTriangles trig
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

baseTriangulation :: [Point] -> (BoundBox Float, Triangulation)
baseTriangulation pts = (bb, foldl' insertTriangle emptyTriangulation [makeTriangle p1 p2 p3, makeTriangle p2 p3 p4])
  where bb@(BoundBox xMin' yMin' xMax' yMax') = boundPoints pts
        [xMin, yMin] = [xMin' - 1, yMin' - 1]
        [xMax, yMax] = [xMax' + 1, yMax' + 1]
        p1 = (xMin, yMin)
        p2 = (xMin, yMax)
        p3 = (xMax, yMin)
        p4 = (xMax, yMax)

getTrianglesAndTriangulation :: Triangulation -> (Triangulation, [Triangle])
getTrianglesAndTriangulation tr = (tr, triangulationToTriangles tr)

triangulationToTriangles :: Triangulation -> [Triangle]
triangulationToTriangles trig = nub $ map sortTrianglePoints $ concatMap (\(p1,nPts) -> map (\(UnorderedPair p2 p3) -> (p1,p2,p3)) (HSet.toList nPts)) ptsWithNeighbors
  where
    pts = HMap.keys trig
    ptsWithNeighbors = map (\pt -> (pt, trig HMap.! pt)) pts

emptyBB = BoundBox 0 0 0 0

doTriangulation :: [Vector] -> (Triangulation, [Triangle], BoundBox Float)
doTriangulation [] = (emptyTriangulation, [], emptyBB)
doTriangulation pts' = case pts of
    (_:_:_:_) ->  (\((a, b), c) -> (a, b, c)) $ (getTrianglesAndTriangulation trig, bb)
    _tooFew   -> (emptyTriangulation, [], emptyBB)
  where
        (bb, baseTrig) = baseTriangulation pts
        trig = addPoints baseTrig pts
        pts  = nub pts'
