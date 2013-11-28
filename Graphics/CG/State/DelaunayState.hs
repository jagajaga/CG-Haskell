{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Graphics.CG.State.DelaunayState where

import           Control.Lens
import           Graphics.CG.Primitives.BoundBox
import           Graphics.CG.Primitives.Triangulation
import           Graphics.Gloss.Data.Vector

fst (a, _, _) = a
snd (_, a, _) = a
thrd (_, _, a) = a

data State = State {
    _points        :: [Vector],
    _triangulation :: Triangulation,
    _boundBox      :: BoundBox Float
} deriving (Show)
makeLenses ''State
