module Matrix34 where

-- this is in fact a 4x4 matrix disguised
-- as a 3x4 matrix for optimization reasons

--
-- The matrix looks like this --
--
-- ax bx cx dx
-- ay by cy dy
-- az bz cz dz
-- 0  0  0  1
--
-- And represented like this -- Matrix34 a b c d
--

import Vector3

data Matrix34 = Matrix34 Vector3 Vector3 Vector3 Vector3 deriving (Show, Read, Eq)

class Matrix m where
  identity :: m
  transform :: m -> Vector3 -> Vector3
  transform3x3 :: m -> Vector3 -> Vector3
  makeRotateX :: Double -> m
  makeRotateY :: Double -> m
  makeRotateZ :: Double -> m
  makeScale :: Double -> Double -> Double -> m

setMatrix34 ax bx cx dx ay by cy dy az bz cz dz = do
  let av = Vector3 ax ay az
  let bv = Vector3 bx by bz
  let cv = Vector3 cx cy cz
  let dv = Vector3 dx dy dz
  Matrix34 av bv cv dv

instance Num Matrix34 where
  (Matrix34 av1 bv1 cv1 dv1) + (Matrix34 av2 bv2 cv2 dv2) = Matrix34 (av1 + av2) (bv1 + bv2) (cv1 + cv2) (dv1 + dv2)
  
  (Matrix34 av1 bv1 cv1 dv1) - (Matrix34 av2 bv2 cv2 dv2) = Matrix34 (av1 - av2) (bv1 - bv2) (cv1 - cv2) (dv1 - dv2)
  
  (Matrix34 (Vector3 ax ay az) (Vector3 bx by bz) (Vector3 cx cy cz) (Vector3 dx dy dz)) * (Matrix34 mva mvb mvc mvd) = do
      let nva = Vector3 ax bx cx
      let nvb = Vector3 ay by cy
      let nvc = Vector3 az bz cz

      let av = Vector3 (dot nva mva) (dot nvb mva) (dot nvc mva)
      let bv = Vector3 (dot nva mvb) (dot nvb mvb) (dot nvc mvb)
      let cv = Vector3 (dot nva mvc) (dot nvb mvc) (dot nvc mvc)
      let dv = Vector3 (dot nva mvd + dx) (dot nvb mvd + dy) (dot nvc mvd + dz)

      Matrix34 av bv cv dv
  
  abs (Matrix34 av bv cv dv) = Matrix34 (abs av) (abs bv) (abs cv) (abs dv)
  
  negate (Matrix34 av bv cv dv) = Matrix34 (negate av) (negate bv) (negate cv) (negate dv)
  
  signum (Matrix34 av bv cv dv) = Matrix34 (signum av) (signum bv) (signum cv) (signum dv)

  fromInteger 1 = Matrix34 xaxis yaxis zaxis origin
  fromInteger v = do
      let vec = fromInteger v :: Vector3
      Matrix34 vec vec vec vec

det22 :: Double -> Double -> Double -> Double -> Double
det22 ax ay bx by = ax*by - ay*bx

instance Matrix Matrix34 where
  identity = Matrix34 xaxis yaxis zaxis origin

  transform (Matrix34 (Vector3 ax ay az) (Vector3 bx by bz) (Vector3 cx cy cz) (Vector3 dx dy dz)) v = do
      let av = Vector3 ax bx cx
      let bv = Vector3 ay by cy
      let cv = Vector3 az bz cz

      Vector3 (dot av v + dx) (dot bv v + dy) (dot cv v + dz)

  transform3x3 (Matrix34 (Vector3 ax ay az) (Vector3 bx by bz) (Vector3 cx cy cz) _) v = do
      let av = Vector3 ax bx cx
      let bv = Vector3 ay by cy
      let cv = Vector3 az bz cz

      Vector3 (dot av v) (dot bv v) (dot cv v)

  makeRotateX angle = do
      let sint = sin(angle)
      let cost = cos(angle)

      Matrix34 xaxis (Vector3 0 cost sint) (Vector3 0 (-sint) cost) origin

  makeRotateY angle = do
      let sint = sin(angle)
      let cost = cos(angle)

      Matrix34 (Vector3 cost 0 (-sint)) yaxis (Vector3 sint 0 cost) origin

  makeRotateZ angle = do
      let sint = sin(angle)
      let cost = cos(angle)

      Matrix34 (Vector3 cost sint 0) (Vector3 (-sint) cost 0) zaxis origin

  makeScale sx sy sz = Matrix34 (scale xaxis sx) (scale yaxis sy) (scale zaxis sz) origin

  
