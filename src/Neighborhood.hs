
module Neighborhood where

import Point

-- Defines a neighborhood algorithm.
--
class Neighborhood n where
  neighborhood :: Point -> [Point]

data FonNeumann

-- For Neumann neighborhood.
instance Neighborhood FonNeumann where
  neighborhood point = map (add point)
    [ Point 0 0   1,  Point 0   1  0, Point   1  0 0
    , Point 0 0 (-1), Point 0 (-1) 0, Point (-1) 0 0
    , Point 0 0   0
    ]
