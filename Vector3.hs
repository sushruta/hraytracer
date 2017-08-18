module Vector3 where

import Data.Monoid
import Data.Group

class (Num a) => Vector a where
  scale :: a -> Double -> a
  dot :: a -> a -> Double
  magnitude2 :: a -> Double
  magnitude :: a -> Double
  cross :: a -> a -> a
  normalize :: a -> a
  distance :: a -> a -> Double
  distance2 :: a -> a -> Double
  magnitude2 x = dot x x
  magnitude v = sqrt $ magnitude2 v
  normalize v = scale v $ 1 / (magnitude v)
  distance2 v1 v2 = do
      let diffv = v1 - v2
      magnitude2 diffv
  distance v1 v2 = sqrt $ distance2 v1 v2

data Vector3 = Vector3 Double Double Double deriving (Eq, Show, Read, Ord)

instance Num Vector3 where
  (Vector3 x1 y1 z1) + (Vector3 x2 y2 z2) = Vector3 (x1 + x2) (y1 + y2) (z1 + z2)
  abs (Vector3 x y z) = Vector3 (abs x) (abs y) (abs z)
  (Vector3 x1 y1 z1) - (Vector3 x2 y2 z2) = Vector3 (x1 - x2) (y1 - y2) (z1 - z2)
  negate v = invert v
  (Vector3 x1 y1 z1) * (Vector3 x2 y2 z2) = Vector3 (x1*x2) (y1*y2) (z1*z2)
  signum (Vector3 x y z) = Vector3 (signum x) (signum y) (signum z)
  fromInteger v = Vector3 (fromInteger v) (fromInteger v) (fromInteger v)

instance Monoid Vector3 where
  mempty = Vector3 0 0 0
  mappend v1 v2 = v1 + v2

instance Group Vector3 where
  invert (Vector3 x y z) = Vector3 (-x) (-y) (-z)

instance Vector Vector3 where
  scale (Vector3 x y z) s = Vector3 (s * x) (s * y) (s * z)
  dot (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = x1*x2 + y1*y2 + z1*z2
  cross (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (y1*z2 - z1*y2) (z1*x2 - z1*z2) (x1*y2 - y1*x2)

xaxis = Vector3 1 0 0
yaxis = Vector3 0 1 0
zaxis = Vector3 0 0 1
origin = Vector3 0 0 0
