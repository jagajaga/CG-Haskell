{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module State.State where

import           Control.Lens
import           Graphics.Gloss

data State = State {
    _points :: [Vector]
}
makeLenses ''State
