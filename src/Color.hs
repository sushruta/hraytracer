module Color where

import Data.Word
import Vector3

white = asColor $ Vector3 1 1 1
grey = asColor $ Vector3 0.25 0.25 0.25
black = asColor $ Vector3 0 0 0
red = asColor $ Vector3 1 0 0
yellow = asColor $ Vector3 1 1 0
blue = asColor $ Vector3 0 0 1
green = asColor $ Vector3 0 0.75 0

type Color = (Word8, Word8, Word8)

-- this method assumes that vector elements are [0, 1)
asColor :: Vector3 -> (Word8, Word8, Word8)
asColor (Vector3 x y z) = (round $ x * 256, round $ y * 256, round $ z * 256)

asColorFn :: (Double -> Double) -> Vector3 -> (Word8, Word8, Word8)
asColorFn f (Vector3 x y z) =
    asColor newVec
  where
    newVec = Vector3 (f x) 0 0