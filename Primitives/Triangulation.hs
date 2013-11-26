module Primitives.Triangulation where

import           Data.HashMap.Strict       (HashMap, empty)
import           Data.HashSet              (HashSet)
import           Graphics.Gloss.Data.Point
import           Primitives.UnorderedPair
type Triangulation = HashMap Point (HashSet (UnorderedPair Point))

emptyTriangulation :: Triangulation
emptyTriangulation = empty
