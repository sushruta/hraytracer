module Ray where

import Vector3

-- Ray is represented as origin and direction
-- r = o + t * d

data Ray = Ray Vector3 Vector3 deriving (Show, Read, Eq)
