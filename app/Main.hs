module Main where

import Graphics.Image (VU(..), readImageRGB, writeImage)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import Text.Read (readMaybe)

import Resize

data Options = Options
  { scaleFactors :: (Int, Int)
  , inPath :: FilePath
  , outPath :: FilePath
  }

main :: IO ()
main = do
  args <- getArgs
  options <- parseOptions args
  inImage <- readImageRGB VU (inPath options)
  let result = resize inImage (scaleFactors options)
  writeImage (outPath options) result

parseOptions :: [String] -> IO Options
parseOptions [i, o, w, h] =
  case readMaybe w of
    Just width
      | width >= 1 && width <= 100 ->
        case readMaybe h of
          Just height
            | height >= 1 && height <= 100 ->
              return $ Options (width, height) i o
          _ -> failWith 3 "Could not parse height (1-100)"
    _ -> failWith 2 "Could not parse width (1-100)"
parseOptions _ =
  failWith 1 "Usage: resize [in-path] [out-path] [width %] [height %]"

failWith :: Int -> String -> IO a
failWith code msg = do
  putStrLn msg
  exitWith (ExitFailure code)
