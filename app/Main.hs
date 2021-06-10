
import Control.Lens hiding (plate)

import Data.Map qualified as Map
import Data.Map (Map)

import Graphics.Gloss.Raster.Field hiding (Point)

import Polysemy
import Polysemy.State

import Liquid
import Draw.Raycast
import Space.Metric
import Space.Edit
import Point
import Queue qualified
import Simulation
import Has
import Neighborhood

import Debug.Trace

main = do
  playField
    (InWindow "Cursed Sands" (640, 480) (200, 200))
    (1, 1)
    10
    (world, Queue.fromList [(Point 23 23 2)])
    (\(w, angle) pt
      -> renderBlock
      $  raycast 30
        ( diagonalRayFrom
        $ fromScreenSpace 100
        $ clampedToCoords (div 640 10, div 480 10)
        $ unskew
        $ pt
        )
        world
    )
    (const id)
    -- (const id)
    (\t -> stepSimulation 200)
  where
    world = empty 30
      & createShape (cube  (Point 6 6 2) 18 (Liquid Nothing))
      & createShape (plate (Point 0 0 0) 30 Rock)
      & createShape (plate (Point 0 0 1) 30 Rock)
      -- & createShape (plate (Point 0 0 2) 10 Rock)
      -- & createShape (plate (Point 0 0 3) 10 Rock)
      -- & createShape (plate (Point 0 0 4) 10 Rock)
      -- & createShape (plate (Point 0 0 5) 10 Rock)
      -- & createShape (plate (Point 0 0 6) 10 Rock)
      -- & createShape (plate (Point 0 0 7) 10 Rock)
      -- & createShape (plate (Point 0 0 8) 10 Rock)
      -- & createShape (plate (Point 0 0 9) 10 Rock)

-- main = print $ raycast
--   (diagonalRayFrom $ fromScreenSpace 99 $ Point2D 33 33)
--   ()

clampedToCoords :: (Int, Int) -> (Float, Float) -> (Float, Float)
clampedToCoords (w, h) (dx, dy) = (unclamp w dx, unclamp h dy)

unclamp :: Int -> Float -> Float
unclamp w x = w' / 2 + w' / 2 * x
  where
    w' = fromIntegral w

renderBlock :: (Int, Cell) -> Color
renderBlock (z, Air)      = rgb 0 0 0
renderBlock (z, Liquid _) = rgb 0 0 (linearFade 100 z 1)
renderBlock (z, Rock)     = rgb (linearFade 100 z 0.5) (linearFade 100 z 0.25) 0.1

linearFade :: Int -> Int -> Float -> Float
linearFade w x f = f * sqr ((w' - x') / w')
  where
    [w', x'] = map fromIntegral [w, x]
    sqr x = x * x

unskew (x, y) = (x - y / 2.35, y * sqrt 1.25)
-- unskew = rotate 135 . unparasha

rotate :: Float -> (Float, Float) -> (Float, Float)
rotate angle (x, y) = (x * c - y * s, x * s + y * c)
  where
    s = sin angle'
    c = cos angle'
    angle' = angle / 180 * pi

-- unparasha (x, y) = (x, y / 2)

removeDuplicates size = filter \pt -> pt == cut size pt

stepSimulation :: Int -> (Volume Cell, Queue.T Point) -> (Volume Cell, Queue.T Point)
stepSimulation fuel (v, q)
  = (\r -> if r == (v, q) then error "nope!" else r)
  $ repack
  $ run
  $ runState   v
  $ evalState (Fuel fuel)
  $ runState  (Tagged Queue.empty :: "next"    =: Queue.T Point)
  $ execState (Tagged q           :: "current" =: Queue.T Point)
  $ sim
  where
    repack (v', (Tagged q1, Tagged q2)) = (v', Queue.append q1 q2)

    sim
      :: Sem
        [ State ("current" =: Queue.T Point)
        , State ("next"    =: Queue.T Point)
        , State Fuel
        , State (Volume Cell)
        ]
        ()
    sim = trace ("sim!" ++ show (Queue.len q)) $ simulate @FonNeumann @Cell @Volume \center around ->
      -- traceShow (around, center) $

      case around? center of
        Liquid Nothing ->
          if isAir (around? down center)
          then Map.fromList [(center, Air), (down center, Liquid Nothing)]

          -- else if isLiquid (around? n center) && isAir (around? s center)
          -- then Map.fromList [(center, Air), (s center, Liquid (Just S))]

          else if isLiquid (around? s center) && isAir (around? n center)
          then Map.fromList [(center, Air), (n center, Liquid (Just N))]

          -- else if isLiquid (around? f center) && isAir (around? w center)
          -- then Map.fromList [(center, Air), (w center, Liquid (Just W))]

          -- else if isLiquid (around? w center) && isAir (around? f center)
          -- then Map.fromList [(center, Air), (f center, Liquid (Just F))]

          else Map.empty

        Liquid (Just v) ->
          if isAir (around? down center)
          then Map.fromList [(center, Air), (down center, Liquid Nothing)]

          else if isAir (around? go v center)
          then Map.fromList [(center, Air), (go v center, Liquid (Just v))]

          else Map.fromList [(center, Liquid Nothing)]

        _ -> Map.empty
