module Primitive where

import Ray
import Intersection
import Vector3
import Intersectable
import Material
import Vertex

intersectionCutoffEpsilon = 0.000001

data Primitive = Triangle Vertex Vertex Vertex Material | Sphere Vector3 Double Material | Plane Vector3 Vector3 Material deriving (Show, Read, Eq)

type Primitives = [Primitive]

instance Intersectable Primitive where
  intersect ray (Triangle v1 v2 v3 mtl) = Nothing

  intersect (Ray o ud) (Plane p0 nrml mtl) = do
    case hitDistance (Ray o ud) (Plane p0 nrml mtl) of
      Just t -> Just $ Intersection pt nrml mtl t
        where pt = o + (scale (normalize ud) t)
      Nothing -> Nothing
    
  intersect (Ray o ud) (Sphere c r mtl) = do
    case hitDistance (Ray o ud) (Sphere c r mtl) of
      Just t -> Just $ Intersection pt nrml mtl t
        where
          pt = o + (scale (normalize ud) t)
          nrml = normalize (pt - c)
      Nothing -> Nothing

  -- intersection code for ray and triangle
  hitDistance ray (Triangle v1 v2 v3 mtl) = Nothing
  
  -- intersection code for ray and plane
  hitDistance (Ray o ud) (Plane p0 n mtl) = do
    let denom = dot n (normalize ud)
    if denom < intersectionCutoffEpsilon
      then Nothing
      else do
        let ptDotN = dot n (p0 - o)
        if ptDotN < 0
        then Nothing
        else Just $ (ptDotN / denom)

  -- intersection code for ray and sphere
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
