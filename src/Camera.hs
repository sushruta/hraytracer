module Camera where

import Vector3
import Matrix34
import Intersection
import Scene
import Color

-- x, y, color
data PixelColor = PixelColor Int Int Color deriving (Show, Read, Eq)
type Image = [PixelColor]

type Resolution = (Int, Int)

-- Resolution, Matrix, FOV, Aspect Ratio
data Camera = Camera Resolution Matrix34 Double Double deriving (Show, Read, Eq)

class CameraType c where
  lookAt :: Vector3 -> Vector3 -> Vector3 -> c
  cameraResolution :: Resolution -> c -> c
  fov :: Double -> c -> c
  aspectRatio :: Double -> c -> c
  pixelToPointIntercepts :: Int -> Int -> c -> Vector3
  -- render :: c -> Scene -> Intersections

instance CameraType Camera where
  aspectRatio ar (Camera r m f _) = Camera r m f ar
  
  fov angle (Camera r m _ ar) = Camera r m (angle * 180.0 / pi) ar
  
  cameraResolution r (Camera _ m fov ar) = Camera r m fov ar
  
  lookAt position target up = do
    let d = position
    let c = normalize $ d - target
    let a = normalize $ cross up c
    let b = cross c a
    let m = Matrix34 a b c d
    let r = (256, 256)
    Camera r m 0.0 0.0
  
  pixelToPointIntercepts x y (Camera (xres, yres) _ fov ar) = do
      let ud = 1.0 -- / (xres - 1) -- - 0.5
      let vd = 1.0 -- / (yres - 1) -- - 0.5
      let wd = 2 * ar * (tan (fov / 2))
      Vector3 ud vd wd
  
  -- render (Camera (xres, yres) mat fov ar) scene = map [pixelToPointIntercepts x y (Camera (xres, yres) mat fov ar) | x <- [0..xres-1], y <- [0..yres-1]]