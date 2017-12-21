module Camera where

import Vector3
import Matrix34
import Intersectable
import Intersection
import Scene
import Ray

import Data.Array.Repa as Repa

type Resolution = (Int, Int)

-- Resolution, Matrix, FOV, Aspect Ratio
data Camera = Camera Resolution Matrix34 Double Double deriving (Show, Read, Eq)

aspectRatio :: Double -> Camera -> Camera
aspectRatio ar (Camera r m f _) = Camera r m f ar

fov :: Double -> Camera -> Camera
fov angle (Camera r m _ ar) = Camera r m (angle * pi / 180.0) ar

cameraResolution :: Resolution -> Camera -> Camera
cameraResolution r (Camera _ m fov ar) = Camera r m fov ar

lookAt :: Vector3 -> Vector3 -> Vector3 -> Camera
lookAt eye target up = do
  let d = eye
  let c = normalize $ d - target
  let a = normalize $ cross up c
  let b = cross c a
  let mat = Matrix34 a b c d
  let res = (256, 256)
  Camera res mat 0.0 0.0

pixelToPoint :: Int -> Int -> Camera -> Vector3
pixelToPoint x y (Camera (xres, yres) (Matrix34 right up back _) fov ar) = do
  let u = (2.0 * (((fromIntegral x) + 0.5) / (fromIntegral xres)) - 1.0) * (tan (fov / 2.0)) * ar
  let v = (1.0 - 2.0 * (((fromIntegral y) + 0.5) / (fromIntegral yres))) * (tan (fov / 2.0))
  let w = (-1.0)
  (scale right u) + (scale up v) + (scale back w)

render :: Camera -> Scene -> Array D DIM2 (Maybe Intersection)
render (Camera (xres, yres) (Matrix34 right up back eye) fov ar) scene =
    Repa.map (\ray -> intersect ray scene) rays
  where
    rays = generateRay (Camera (xres, yres) (Matrix34 right up back eye) fov ar) eye xres yres

generateRay :: Camera -> Vector3 -> Int -> Int -> Array D DIM2 Ray
generateRay camera eye width height =
    Repa.fromFunction (Z:.height:.width) rayFn
  where
    rayFn (Z:.i:.j) = Ray eye (target - eye)
      where
        target = pixelToPoint j i camera
