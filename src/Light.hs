module Light where

import Vector3
import Color

type Lights = [Light]

data Light = LambertianLight Color deriving (Show, Read)
