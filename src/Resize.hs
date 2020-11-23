module Resize
  ( resize
  ) where

import Control.Monad (join)
import qualified Graphics.Image as I

type Image = I.Image I.VU I.RGB Double

type Pixel = I.Pixel I.RGB Double

resize :: Image -> (Int, Int) -> Image
resize image (wFactor, hFactor) = energies image

energies :: Image -> Image
energies image = I.imap energy image
  where
    findPixel = I.defaultIndex (I.PixelRGB 1 1 1) image
    energy (x, y) p =
      let left = findPixel (x - 1, y)
          right = findPixel (x + 1, y)
          above = findPixel (x, y - 1)
          below = findPixel (x, y + 1)
          value =
            sqrt
              (pixelDifferenceSquared left right +
               pixelDifferenceSquared above below)
      in I.PixelRGB value value value

pixelDifferenceSquared :: Pixel -> Pixel -> Double
pixelDifferenceSquared (I.PixelRGB r0 g0 b0) (I.PixelRGB r1 g1 b1) =
  join (*) . sum . map abs $ [r0 - r1, g0 - g1, b0 - b1]
