
module Draw.Raycast where

import Control.Lens
import Control.Monad

import Data.Default
import Data.List

import Space.Metric
import Point

diagonalRayFrom :: Point -> [Point]
diagonalRayFrom = unfoldr \pt -> do
  guard
    (  Point.x pt >= 0
    && Point.y pt >= 0
    && Point.z pt >= 0
    )
  return
    ( pt
    , pt
      { x = Point.x pt - 1
      , y = Point.y pt - 1
      , z = Point.z pt - 1
      }
    )

data Point2D = Point2D { x, y :: Int }
  deriving stock (Eq, Ord, Show)

fromScreenSpace :: Int -> (Float, Float) -> Point
fromScreenSpace diagNormal (x, y) = Point
  x' y' (diagNormal - x' - y')
  where
    x' = round x
    y' = round y

raycast :: (Metric spc, Default c, Eq c) => Int -> [Point] -> spc c -> (Int, c)
raycast size pts spc = run (0, def) pts
  where
    run c      []         = c
    run (z, c) (pt : pts)
      | c  /= def         = (z, c)
      | pt /= cut size pt = run (z + 1, c) pts
      | otherwise         = run (z + 1, spc^.atCell pt) pts
