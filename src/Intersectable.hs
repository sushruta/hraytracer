module Intersectable where

import Ray
import Intersection
import Vector3

class Intersectable p where
  intersect :: Ray -> p -> Maybe Intersection
  hitDistance :: Ray -> p -> Maybe Double