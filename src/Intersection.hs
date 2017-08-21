module Intersection where

import Data.List

import Vector3
import Material

data Intersection = Intersection Vector3 Material Double deriving (Show, Read, Eq, Ord)

cmpFn :: Intersection -> Intersection -> Ordering
cmpFn (Intersection _ _ d1) (Intersection _ _ d2)
  | d1 < d2  = LT
  | d1 > d2  = GT
  | d1 == d2 = EQ

closestIntersection [] = Nothing
closestIntersection [intersection] = Just intersection
closestIntersection intersections = do
    let sortedIntersections = sortBy cmpFn intersections
    Just (head sortedIntersections)
