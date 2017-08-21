module Main where

import Object
import Material
import Color

main = do
    let obj1 = makeBox 5 0.1 5 (LambertMaterial green)
    putStrLn $ show obj1
