{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Graphics.CG.State.State where

import           Control.Lens
import           Graphics.Gloss

data State = State {
    _points :: [Vector]
}
makeLenses ''State
