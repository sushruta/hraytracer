module Scene where

import Color
import Intersectable
import Intersection
import Object
import Light
import Ray
import Util

type SkyColor = Color

data Scene = Scene Objects Lights SkyColor deriving (Show, Read)

addObject :: Scene -> Object -> Scene
addObject (Scene objects lights skyColor) object = Scene (objects ++ [object]) lights skyColor

addLight :: Scene -> Light -> Scene
addLight (Scene objects lights skyColor) light = Scene objects (lights ++ [light]) skyColor

addSkyColor :: Scene -> Color -> Scene
addSkyColor (Scene objects lights skyColor) sc = Scene objects lights sc

instance Intersectable Scene where
    intersect ray (Scene objects _ _) = closestIntersection $ filterJust $ map (intersect ray) objects
    hitDistance (Ray o ud) scene = Nothing