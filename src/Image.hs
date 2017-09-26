module Image where

import Data.Word
import Intersection

import Data.Word
import Data.Array.Repa as Repa
import Data.Array.Repa.IO.BMP as BMP

import qualified Data.Vector.Unboxed as UBV
import System.Random as Rand
import System.Random.Mersenne as Mersenne
import Control.Monad

import Camera
import Color

toWord8Vector :: UBV.Vector Double -> UBV.Vector Word8
toWord8Vector = UBV.map (\v -> round $ v * 256)

genNumbers :: Int -> [Double] -> UBV.Vector Double
genNumbers n rs = UBV.fromList (take n rs)

generateRandomImage :: Int -> Int -> [Double] -> Array U DIM2 Color
generateRandomImage w h rs =
    computeS (fromFunction (Z:.w:.h) imgFn)
  where
    channels = 3 :: Int
    mat = Repa.fromUnboxed (Z:.w:.h:.channels) (toWord8Vector (genNumbers (w*h*channels) rs))
    imgFn (Z:.i:.j) = (mat ! (Z:.i:.j:.0), mat ! (Z:.i:.j:.1), mat ! (Z:.i:.j:.2))

generateBlankImage :: Int -> Int -> Array D DIM2 Color
generateBlankImage w h =
    fromFunction (Z:.h:.w) (\(Z:.i:.j) -> (0, 0, 0))

gradientImage :: Int -> Int -> Camera -> Array D DIM2 Color
gradientImage width height camera = do
    Repa.fromFunction (Z:.width:.height) colorFn
  where
    colorFn (Z:.i:.j) = asColorFn (\v -> v + 0.5) offsets
      where
        offsets = pixelToOffset i j camera