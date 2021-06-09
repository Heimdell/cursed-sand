
module Space.Metric where

import Control.Lens

import Data.Default
import Data.Map qualified as Map
import Data.Map (Map)

import Point

class Metric s where
  atCell      :: Default a => Point -> Lens' (s a) a
  updateSpace :: (Default c, Metric s) => Map Point c -> s c -> s c
  updateSpace = flip $ Map.foldrWithKey (\pt c -> atCell pt .~ c)

-- A cubical volume of space.
--
data Volume a = Volume
  { edge  :: Int
  , cells :: Map Point a
  }
  deriving stock (Eq, Ord, Show)

-- The metric for the cube is cyclic across all 3 axises.
--
instance Metric Volume where
  atCell p = lens get set
    where
      get   (Volume edge cells)   = maybe def id $ Map.lookup (cut edge p) cells
      set v@(Volume edge cells) c = v { cells = Map.insert (cut edge p) c cells }

  updateSpace m v = v { cells = Map.union m (cells v) }
