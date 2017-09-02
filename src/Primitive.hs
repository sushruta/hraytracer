module Primitive where

import Ray
import Intersection
import Vector3
import Intersectable
import Material
import Vertex

intersectionCutoffEpsilon = 0.000001

data Primitive = Triangle Vertex Vertex Vertex Material | Sphere Vector3 Double Material deriving (Show, Read, Eq)

type Primitives = [Primitive]

instance Intersectable Primitive where
  intersect ray (Triangle v1 v2 v3 mtl) = Nothing
  intersect (Ray o ud) (Sphere c r mtl) = do
    case hitDistance (Ray o ud) (Sphere c r mtl) of
      Just t -> Just $ Intersection (o + scale (normalize ud) t) mtl t
      Nothing -> Nothing

  hitDistance ray (Triangle v1 v2 v3 mtl) = Nothing
  hitDistance (Ray o ud) (Sphere c r mtl) = do
    let d = normalize ud
    let co = c - o
    let dco = dot d co
    let mco = magnitude2 co
    let disc = dco*dco - mco + r*r
    if disc < 0
      then Nothing
      else do
        let t = dot d co + sqrt disc
        if t < intersectionCutoffEpsilon
          then Nothing
          else Just t