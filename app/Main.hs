module Main where

import Object
import Material
import Color
import Ray
import Intersectable
import Primitive
import Vector3
import Camera
import Light
import Ray
import Scene
import Image
import Shade

import Data.Array.Repa as Repa

main = do
    let width = 640
    let height = 480

    let sphere1 = Sphere (Vector3 0 0 2) 0.5 (LambertMaterial green)

    let sphere2 = Sphere (Vector3 0 1 3) 0.5 (LambertMaterial red)

    let plane1 = Plane (Vector3 1 (-4) 1) (normalize (Vector3 0 (-1) 0)) (LambertMaterial grey)

    let obj2 = PrimitiveList [sphere1, plane1, sphere2]

    let skyColor = asColor $ Vector3 0.8 0.9 1.0
    let scene = Scene [obj2] [LambertianLight white] skyColor

    let cam = cameraResolution (width, height)
            $ aspectRatio ((fromIntegral width) / (fromIntegral height))
            $ fov 45.0
            $ lookAt (Vector3 0 0 0) (Vector3 0 0 1) (Vector3 0 1 0)

    let intersections = render cam scene

    let flatshade = FlatShade "flat style"
    let bitmapshade = BitmapShade "bitmap style"

    let img = Repa.map (computeReflectance flatshade) intersections

    saveAsBmpImage width height img "myout.bmp"

    putStrLn "done"
