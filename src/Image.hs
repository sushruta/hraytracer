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

saveAsBmpImage :: Int -> Int -> Array D DIM2 Color -> String -> IO ()
saveAsBmpImage w h img filename = do
  -- invert the image because that's how bmp stores it :shrug:
  let bimg = fromFunction (Z:.h:.w) (\(Z:.i:.j) -> img ! (Z:.(h - i - 1):.j))
  BMP.writeImageToBMP filename (computeS bimg)

generateBlankImage :: Int -> Int -> Array D DIM2 Color
generateBlankImage w h =
    fromFunction (Z:.h:.w) (\(Z:.i:.j) -> (0, 0, 0))
