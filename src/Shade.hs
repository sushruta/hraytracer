module Shade where

import Data.Word

import Vector3
import Intersection
import Material
import Color

class Shadeable m where
  computeReflectance :: m -> Maybe Intersection -> Vector3 -> Vector3 -> Color

-- instance Material Shadeable where
--  computeReflectance m i inp out = Vector3 1 2 3 :: Color

--instance BitmapShade Shadeable where
--   computeReflectance bitmap 

shadeBitmap :: Maybe Intersection -> (Word8, Word8, Word8)
shadeBitmap Nothing = (0, 0, 0)
shadeBitmap _ = (255, 255, 255)