module Resize
  ( resize
  ) where

import Data.Maybe (fromJust)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Graphics.Image as I

type Image c = I.Image I.VU c Double

type Pixel c = I.Pixel c Double

resize :: Image I.RGB -> (Int, Int) -> Image I.RGB
resize image (wFactor, hFactor) =
  I.fromLists $ applyN hSeams removeSeam (I.toLists image)
  where
    hSeams = nSeams hFactor (I.cols image)

nSeams :: Int -> Int -> Int
nSeams percent size =
  floor $ (fromIntegral (100 - percent) / 100) * fromIntegral size

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
deltaSquared p1 p2 = sum (abs (p1 - p2)) ** 2

data Direction
  = L
  | R
  | A
  deriving (Eq)

energyMap :: Image I.Y -> [Vector (Pixel I.Y, Direction)]
energyMap image = go [] (reverse (I.toLists image))
  where
    size = I.cols image
    go result [] = result
    go [] (row:rest) = go [V.fromList (zip row (repeat A))] rest
    go seen@(prev:_) (row:rest) =
      go (V.imap (combine prev) (V.fromList row) : seen) rest
    combine prev i p =
      let left = fst (prev ! max (i - 1) 0)
          above = fst (prev ! i)
          right = fst (prev ! min (i + 1) (size - 1))
      in if left < above && left < right
           then (p + left, L)
           else if above < right
                  then (p + above, A)
                  else (p + right, R)

removeSeam :: [[Pixel I.RGB]] -> [[Pixel I.RGB]]
removeSeam image =
  go [] (fromJust $ V.elemIndex (minimum firstRow) firstRow) eMap image
  where
    eMap = energyMap (energies (I.fromLists image))
    firstRow = fmap fst (head eMap)
    go result n (e:es) (row:rs) =
      let n' =
            case snd (e ! n) of
              L -> n - 1
              R -> n + 1
              A -> n
      in go (deleteIndex n row : result) n' es rs
    go result _ _ _ = reverse result

deleteIndex :: Int -> [a] -> [a]
deleteIndex i xs = ys ++ drop 1 zs
  where
    (ys, zs) = splitAt i xs

applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ a = a
applyN n f a = applyN (n - 1) f (f a)
