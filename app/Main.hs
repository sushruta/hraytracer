module Main where

import Object
import Material
import Color
import Ray
import Intersectable
import Primitive
import Vector3
import Camera

main = do
    let obj1 = makeBox 5 0.1 5 (LambertMaterial green)
    putStrLn $ show obj1

    let sphere1 = Sphere (Vector3 0 0 0) 2 (LambertMaterial green)
    let r1 = Ray (Vector3 (-5) (-5) (-5)) (Vector3 1 1 1)
    putStrLn $ show $ intersect r1 sphere1

    -- let cam = cameraResolution (64, 64) $ aspectRatio 1.33 $ fov 40.0 $ lookAt (Vector3 2 2 5) (Vector3 0 0 0) (Vector3 0 1 0)
    let cam1 = lookAt (Vector3 2 2 5) (Vector3 0 0 0) (Vector3 0 1 0)
    putStrLn $ show cam1