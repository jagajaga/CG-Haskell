module Algorithms.Rotate (clockwise) where

import Graphics.Gloss.Data.Vector

clockwise :: Vector -> Vector -> Vector -> Bool
clockwise o a b = (a `sub` o) `cross` (b `sub` o) <= 0

cross :: Vector -> Vector -> Float
cross (x1, y1) (x2, y2) = x1 * y2 - x2 * y1

sub :: Vector -> Vector -> Vector
sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
