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
import Data.Array.Repa.IO.BMP as BMP

main = do
    let width = 400
    let height = 200

    -- let obj1 = makeBox 5 0.1 5 (LambertMaterial green)
    -- putStrLn $ show obj1

    let sphere1 = Sphere (Vector3 0 0 4) 0.5 (LambertMaterial green)

    let obj2 = PrimitiveList [sphere1]

    let skyColor = asColor $ Vector3 0.8 0.9 1.0
    let scene = Scene [obj2] [LambertianLight white] skyColor

    let cam = cameraResolution (width, height) $ aspectRatio 1.0 $ fov 90.0 $ lookAt (Vector3 0 0 0) (Vector3 0 0 1) (Vector3 0 1 0)
    let intersections = render cam scene

    let img = Repa.map (shadeBitmap) intersections
    BMP.writeImageToBMP "myout.bmp" (computeS img)

    -- BMP.writeImageToBMP "myout.bmp" (computeS $ gradientImage width height cam)
    putStrLn "done"