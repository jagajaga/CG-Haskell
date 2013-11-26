{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module State.DelaunayState where

import           Primitives.Triangulation
import           Control.Lens
import           Graphics.Gloss.Data.Vector

data State = State {
    _points        :: [Vector],
    _triangulation :: Triangulation
}
makeLenses ''State
