module Graphics.CG.Primitives.BoundBox(BoundBox (..)
                                       , boundPoints
                                       , pointInBoundBox
                                       , boundPointsLines
                                       , obb
                                       , getBoundBoxCenter) where

import           Graphics.CG.Algorithms.ConvexHull.Andrew
import           Graphics.CG.Algorithms.Rotate
import           Graphics.Gloss.Data.Point
import           Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Line
import           Data.Array
import           Data.Function
import           Data.List
import           Data.Maybe

import           Debug.Trace

data BoundBox a = BoundBox !a !a !a !a deriving (Show, Eq)

boundPoints :: Ord a => [(a, a)] -> BoundBox a
boundPoints ps =
  let
    xs = map fst ps
    ys = map snd ps
  in BoundBox (minimum xs) (minimum ys) (maximum xs) (maximum ys)

boundPointsLines :: BoundBox Float -> [Vector]
boundPointsLines (BoundBox a b c d) = [(a,b), (c, d)]

pointInBoundBox :: Point -> BoundBox Float -> Bool
pointInBoundBox pt (BoundBox a b c d) = pointInBox pt (a, b) (c, d)

distPoints :: Floating a => (a, a) -> (a, a) -> a
distPoints (x1, y1) (x2, y2) = sqrt $ (x1 -x2) ^ 2 + (y1 - y2) ^ 2

distVec :: Floating a => (a, a) -> (a, a) -> (a, a) -> a
distVec (x1, y1) (x2, y2) (x0, y0) = d / v where
    d = abs $ (x2 - x1) * (y1 - y0) - (x1 - x0) * (y2 - y1)
    v = sqrt $ (x2 -x1) ^ 2 + (y2 - y1) ^ 2

rotateCaliper :: (Integral t, Ix t) => Array t (Float, Float) -> Float -> [t] -> [(Float, Float)] -> (Float, ((Float, Float), (Float, Float), (Float, Float), (Float, Float), ((Float, Float), (Float, Float)))) -> t -> ((Float, Float), (Float, Float), (Float, Float), (Float, Float), ((Float, Float), (Float, Float)))
rotateCaliper arr ang [ pa, pb, qa, qb ] [ cpa, cpb, cqa, cqb ] (area, (rpa, rpb, rqa, rqb, rsaved)) n
    | 2 * ang > pi = (rpa, rpb, rqa, rqb, rsaved)
    | otherwise = rotateCaliper arr ang' [ pa', pb', qa', qb' ] [ cpa', cpb', cqa', cqb' ] (area', (rpa', rpb', rqa', rqb', rsaved')) n where

    p1@(x1, y1) = arr ! pa
    p2@(x2, y2) = arr ! (mod (pa + 1) n)
    p3@(x3, y3) = arr ! pb
    p4@(x4, y4) = arr ! (mod (pb + 1) n)

    q1@(x5, y5) = arr ! qa
    q2@(x6, y6) = arr ! (mod (qa + 1) n)
    q3@(x7, y7) = arr ! qb
    q4@(x8, y8) = arr ! (mod (qb + 1) n)

    t1 = angleVV cpa ((x2 - x1), (y2 - y1))
    t2 = angleVV cpb ((x4 - x3), (y4 - y3))
    t3 = angleVV cqa ((x6 - x5), (y6 - y5))
    t4 = angleVV cqb ((x8 - x7), (y8 - y7))
    t = minimum [ t1, t2, t3, t4 ]

    cpa' = rotateV t cpa
    cpb' = rotateV t cpb
    cqa' = rotateV t cqa
    cqb' = rotateV t cqb

    ang' = ang + t
    (pa', pb', qa', qb', saved') = fN1 [ t1, t2, t3, t4 ] t where
        fN1 [ t1, t2, t3, t4 ] t
           | t == t1 = (mod (pa + 1) n, pb, qa, qb, (p1, p2))
           | t == t2 = (pa, mod (pb + 1) n, qa, qb, (p3, p4))
           | t == t3 = (pa, pb, mod (qa + 1) n, qb, (q1, q2))
           | otherwise = (pa, pb, qa, mod (qb + 1) n, (q3, q4))

    (length', width') = fN2 [ t1, t2, t3, t4 ] t where
        fN2 [ t1, t2, t3, t4 ] t
           | t == t1 = (distVec (arr ! pa) (arr ! pa') (arr ! pb), distPoints (arr ! qa') (arr ! qb'))
           | t == t2 = (distVec (arr ! pb) (arr ! pb') (arr ! pa), distPoints (arr ! qa') (arr ! qb'))
           | t == t3 = (distVec (arr ! qa) (arr ! qa') (arr ! qb), distPoints (arr ! pa') (arr ! pb'))
           | otherwise = (distVec (arr ! qb) (arr ! qb') (arr ! qa), distPoints (arr ! pa') (arr ! pb'))

    area' = (min area $ length' * width')
    (rpa', rpb', rqa', rqb', rsaved') = if area' < area then  (arr ! pa', arr ! pb', arr ! qa', arr ! qb', saved') else (rpa, rpb, rqa, rqb, rsaved)

obb :: [Vector] -> [Vector]
obb s = case vs of
    a | length a <= 3 -> []
    p -> result where
       y1@(_, y3) = minimumBy (on compare snd) p
       y2@(_, y4) = maximumBy (on compare snd) p
       x1@(x5, _) = minimumBy (on compare fst) p
       x2@(x6, _) = maximumBy (on compare fst) p
       pa = fromJust . findIndex (== y1) $ p
       pb = fromJust . findIndex (== y2) $ p
       qa = fromJust . findIndex (== x1) $ p
       qb = fromJust . findIndex (== x2) $ p
       cpa = (1, 0)
       cpb = ((-1), 0)
       cqa = (0, (-1))
       cqb = (0, 1)
       area = abs $ (y4 - y3) * (x6 - x5)
       n = length p
       arr = listArray (0, n - 1) p
       (a, b, c, d, prev@(s, t)) = rotateCaliper arr 0 [pa, pb, qa, qb] [cpa, cpb, cqa, cqb] (area, (y1, y2, x1, x2, (x2, x1))) n 
       result = createBoundBoxOfLines
       createBoundBoxOfLines = box where
        firstDot  
            | t == a = closestPointOnLine s t c
            | t == b = closestPointOnLine s t d
            | t == c = closestPointOnLine s t a
            | t == d = closestPointOnLine s t b
        d2 
            | t == a = closestPointOnLine firstDot c b
            | t == b = closestPointOnLine firstDot d a
            | t == c = closestPointOnLine firstDot a d
            | t == d = closestPointOnLine firstDot b c
        d3 
            | t == a = closestPointOnLine d2 b d
            | t == b = closestPointOnLine d2 a c
            | t == c = closestPointOnLine d2 d a
            | t == d = closestPointOnLine d2 c b
        d4 
            | t == a = closestPointOnLine d3 d a
            | t == b = closestPointOnLine d3 c b
            | t == c = closestPointOnLine d3 a c
            | t == d = closestPointOnLine d3 b d
        box = [firstDot, d2, d3, d4]
    where vs = convexHullAndrew s

areaBB :: BoundBox Float -> Float
areaBB bb = w * h where
    [p1, p2, p3, p4] = createLines bb
    w = dotV p1 p2
    h = dotV p4 p3

createLines :: BoundBox Float -> [Vector]
createLines (BoundBox xMin' yMin' xMax' yMax') = [p2, p1, p3, p4] where
        [xMin, yMin] = [xMin' - 1, yMin' - 1]
        [xMax, yMax] = [xMax' + 1, yMax' + 1]
        p1 = (xMin, yMin)
        p2 = (xMin, yMax)
        p3 = (xMax, yMin)
        p4 = (xMax, yMax)

getBoundBoxCenter :: BoundBox Float -> Vector
getBoundBoxCenter bb = center where
    [(x1, y1), (x2, y2)] = boundPointsLines bb
    xCenter = (x1 + x2)/2
    yCenter = (y1 + y2)/2
    center = (xCenter, yCenter)
