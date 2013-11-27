{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module State.DelaunayState where

import           Control.Lens
import           Graphics.Gloss.Data.Vector
import           Primitives.BoundBox
import           Primitives.Triangulation

fst (a, _, _) = a
snd (_, a, _) = a
thrd (_, _, a) = a

data State = State {
    _points        :: [Vector],
    _triangulation :: Triangulation,
    _boundBox      :: BoundBox Float
} deriving (Show)
makeLenses ''State
