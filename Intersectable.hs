module Intersectable where

import Ray
import Intersection

class Intersectable p where
  intersect :: Ray -> p -> Maybe Intersection
