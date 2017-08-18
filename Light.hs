module Light where

import Color

type Lights = [Light]

data Light = LambertianLight Color deriving (Show, Read)
