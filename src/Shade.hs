module Shade where

import Data.Word

import Vector3
import Intersection
import Material
import Color

data ShadingTechniques = FlatShade String | BitmapShade String deriving (Show, Read, Eq)

class Shadeable m where
  computeReflectance :: m -> Maybe Intersection -> (Word8, Word8, Word8)

instance Shadeable ShadingTechniques where
  computeReflectance (BitmapShade _) Nothing = (0, 0, 0)
  computeReflectance (BitmapShade _) _ = (255, 255, 255)
  
  computeReflectance (FlatShade _) intersection = do
    case intersection of
      Just (Intersection pt nrml (LambertMaterial c) t) -> c
      Just (Intersection pt nrml (SpecularMaterial c1 c2) t) -> c1
      Nothing -> (0, 0, 0)

-- shadeBitmap :: Maybe Intersection -> (Word8, Word8, Word8)
-- shadeBitmap Nothing = (0, 0, 0)
-- shadeBitmap _ = (255, 255, 255)
