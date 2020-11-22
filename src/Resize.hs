module Resize
  ( resize
  ) where

import qualified Graphics.Image as Image

resize ::
     (Image.Array a b c) => Image.Image a b c -> (Int, Int) -> Image.Image a b c
resize image (wFactor, hFactor) =
  Image.resize Image.Bilinear Image.Edge (w', h') image
  where
    (w, h) = Image.dims image
    w' = scale w wFactor
    h' = scale h hFactor
    scale n p = floor (fromIntegral n * fromIntegral p / (100 :: Double))
