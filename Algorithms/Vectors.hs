{-# LANGUAGE BangPatterns #-}
module Algorithms.Vectors where

import           Graphics.Gloss.Data.Vector

angle :: Vector -> Vector -> Vector -> Float
angle !pA !pC !pB = if gamma > pi then pi - gamma else gamma
  where
    gamma = abs $ acos ((a*a + b*b - c*c) / (2 * a * b))
    a = magV (pC - pB)
    b = magV (pA - pC)
    c = magV (pA - pB)
