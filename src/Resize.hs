module Resize
  ( resize
  ) where

import Control.Monad (join)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Graphics.Image as I

type Image c = I.Image I.VU c Double

type Pixel c = I.Pixel c Double

resize :: Image I.RGB -> (Int, Int) -> Image I.RGB
resize image (vFactor, hFactor) =
  I.toImageRGB . fst $ energyMap (energies image)
  where
    vSeams = nSeams vFactor (I.rows image)
    hSeams = nSeams hFactor (I.cols image)

nSeams :: Int -> Int -> Int
nSeams percent size = floor (fromIntegral (100 - percent) / 100) * size

energies :: Image I.RGB -> Image I.Y
energies image = I.imap energy image
  where
    findPixel = I.defaultIndex (I.PixelRGB 1 1 1) image
    energy (x, y) p =
      let left = findPixel (x - 1, y)
          right = findPixel (x + 1, y)
          above = findPixel (x, y - 1)
          below = findPixel (x, y + 1)
          value = sqrt (deltaSquared left right + deltaSquared above below)
      in I.PixelY value

deltaSquared :: Pixel I.RGB -> Pixel I.RGB -> Double
deltaSquared (I.PixelRGB r0 g0 b0) (I.PixelRGB r1 g1 b1) =
  join (*) . sum . map abs $ [r0 - r1, g0 - g1, b0 - b1]

energyMap :: Image I.Y -> (Image I.Y, Map (Int, Int) Int)
energyMap image = go [] Map.empty (reverse (I.toLists image))
  where
    go seen directions [] = (I.normalize (I.fromLists seen), directions)
    go [] directions (row:rest) = go [row] directions rest
    go seen@(prev:_) directions (row:rest) =
      go (zipWith (combine prev) row [0 ..] : seen) directions rest
    combine prev p i =
      p + minimum (take (min 3 (2 + i)) (drop (max 0 (i - 1)) prev))
