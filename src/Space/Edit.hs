
module Space.Edit where

import Control.Lens

import Data.Default
import Data.Foldable (traverse_)

import Polysemy
import Polysemy.State

import Point
import Space.Metric

-- Shape generator, produces a 1-thick square plate.
--
plate :: Point -> Int -> c -> [(Point, c)]
plate pt edge c =
  [ (pt `add` Point x y 0, c)
  | x <- diapazone
  , y <- diapazone
  ]
  where
    diapazone = [0.. edge - 1]

-- Shape generator, produces a cube.
--
cube :: Point -> Int -> c -> [(Point, c)]
cube pt edge c =
  [ (pt `add` Point x y z, c)
  | x <- diapazone
  , y <- diapazone
  , z <- diapazone
  ]
  where
    diapazone = [0.. edge - 1]

-- Place shape into the world.
--
createShape :: forall c spc. (Default c, Metric spc) => [(Point, c)] -> spc c -> spc c
createShape = flip $ foldl $ flip $ uncurry createBlock

-- Place single block into the world.
--
createBlock :: forall c spc. (Default c, Metric spc) => Point -> c -> spc c -> spc c
createBlock pt c = do
  atCell pt .~ c
