
module Liquid where

import Data.Default

import Point

-- Block state.
--
data Cell
  = Liquid (Maybe Dir)  -- horizontal flow
  | Rock
  | Air
  deriving stock (Eq, Ord, Show)

-- Direction of flow.
--
data Dir = N | S | F | W
  deriving stock (Eq, Ord, Show)

instance Default Cell where def = Air

isLiquid :: Cell -> Bool
isLiquid Liquid {} = True
isLiquid _         = False

isAir :: Cell -> Bool
isAir Air {} = True
isAir _      = False

go :: Dir -> Point -> Point
go N = n
go S = s
go W = w
go F = f
