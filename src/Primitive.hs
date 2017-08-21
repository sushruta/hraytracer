module Primitive where

import Ray
import Intersection
import Vector3
import Intersectable
import Material
import Vertex

data Primitive = Triangle Vertex Vertex Vertex Material | Sphere Vector3 Double Material deriving (Show, Read, Eq)

type Primitives = [Primitive]

instance Intersectable Primitive where
  intersect ray (Triangle v1 v2 v3 mtl) = Nothing
  intersect ray (Sphere c1 r mtl) = Nothing