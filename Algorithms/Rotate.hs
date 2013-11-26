module Algorithms.Rotate (Rotate(..), orientation, clockwise) where

import           Graphics.Gloss.Data.Vector
import           Numeric.Limits

data Rotate = Clockwise | Parallel | Counterclockwise
    deriving (Show, Eq)

instance Enum Rotate where
    toEnum a = case a of
        -1 -> Counterclockwise
        1 -> Clockwise
        0 -> Parallel
    fromEnum a = case a of
        Counterclockwise -> -1
        Clockwise -> 1
        Parallel -> 0

orientation :: Vector -> Vector -> Vector -> Rotate
orientation (ax, ay) (bx, by) (cx, cy)
    | cab == 0 = Parallel
    | cab > eps = Counterclockwise
    | cab < -eps =  Clockwise
    where
        l = (bx - ax) * (cy - ay)
        r = (by - ay) * (cx - ax)
        cab = l - r
        eps = (abs l + abs r) * 8 * epsilon

clockwise :: Vector -> Vector -> Vector -> Bool
clockwise o a b = (a `sub` o) `cross` (b `sub` o) <= 0

cross :: Vector -> Vector -> Float
cross (x1, y1) (x2, y2) = x1 * y2 - x2 * y1

sub :: Vector -> Vector -> Vector
sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
