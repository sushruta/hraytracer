module Material where

import Vector3
import Color

data Material = LambertMaterial Color | SpecularMaterial Color Color deriving (Show, Read, Eq, Ord)
