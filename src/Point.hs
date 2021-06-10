
module Point where

-- A 3D point.
--
data Point = Point { x, y, z :: Int }
  deriving stock (Eq, Ord, Show)

-- Inject point into toroidal metric.
--
cut :: Int -> Point -> Point
cut s (Point x y z) = Point (mod x s) (mod y s) (mod z s)

scalar :: (Int -> Int -> Int) -> Point -> Point -> Point
scalar (?) (Point x y z) (Point a b c) = Point (x ? a) (y ? b) (z ? c)

add :: Point -> Point -> Point
add = scalar (+)

sub :: Point -> Point -> Point
sub = scalar subtract

up   = add $ Point   0   0   1
down = add $ Point   0   0 (-1)
n    = add $ Point   1   0   0
s    = add $ Point (-1)  0   0
f    = add $ Point   0   1   0
w    = add $ Point   0 (-1)  0
