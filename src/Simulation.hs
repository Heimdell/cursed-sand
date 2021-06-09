
module Simulation where

import Control.Lens
import Control.Monad

import Data.Default
import Data.Traversable (for)
import Data.List
import Data.Map qualified as Map
import Data.Map (Map)

import Polysemy
import Polysemy.State

import Has
import Queue
import Point
import Neighborhood
import Space.Metric

-- Remaining updates for this turn.
--
newtype Fuel = Fuel Int
  deriving newtype (Eq, Ord, Show, Num)

-- Run a turn of updates.
--
-- Will stop if run out of fuel or elements of "current" queue.
--
-- Everything that moves that turn goes to "next" queue.
--
-- User is expected to `refuel` and squash @queue "current" <> queue "next"@ into
-- @queue "current" before calling it again (at least if @"current"@ is empty).
--
simulate
  :: forall n c spc m
  .  ( Default c
     , Members
         [ State ("current" =: Queue.T Point)  -- queue for curent turn
         , State ("next"    =: Queue.T Point)  -- queue for next turn
         , State Fuel                          -- update limiter
         , State (spc c)                       -- a space...
         ]
         m
     , Metric spc                          -- ... with a metric
     , Neighborhood n                      -- a neighborhood
     )
  => (Map Point c -> Map Point c)          -- gets neighbors, returns changed only
  -> Sem m ()
simulate step = run
  where
    run = do
      whileHasFuel do
        gets' @"current" Queue.pop >>= \case
          Nothing -> return Stop

          Just (rest, point :: Point) -> do
            let neighbors = neighborhood @n point  -- get all points around locus

            pts <- for neighbors \pt -> do         -- construct subspace
              c <- gets @(spc c) (^.atCell pt)
              return (pt, c)

            let changedCut = step (Map.fromList pts)        -- perform step over subspace
            let changed    = Map.keys changedCut            -- get points that changed
            let poked      = (neighborhood @n =<< changed)  -- neighbors of all changed...
                           \\ changed                       -- ... except changed themselves

            modify   @(spc c)   $ updateSpace changedCut    -- update space
            put'     @"current" $ Queue.pushAll poked rest  -- simulate touched neighbors this turn
            modify'' @"next"    $ Queue.pushAll changed     -- simulate changed neighbors next turn

            return Continue -- continue

data Continue = Continue | Stop

outOfFuel :: Member (State Fuel) m => Sem m Bool
outOfFuel = do
  Fuel n <- get
  return $ n == 0

whileHasFuel :: Member (State Fuel) m => Sem m Continue -> Sem m ()
whileHasFuel action = loop
  where
    loop = do
      done <- outOfFuel
      unless done do
        modify $ subtract (Fuel 1)
        continue <- action
        case continue of
          Continue -> loop
          Stop     -> return ()

refuel :: Member (State Fuel) m => Fuel -> Sem m ()
refuel = put
