module Shade where

import Vector3
import Intersection
import Material

class Shadeable m where
  computeReflectance :: m -> Intersection -> Vector3 -> Vector3 -> Color

instance Material Shadeable where
  computeReflectance m i in out = Vector3 1 2 3 :: Color
