
module Main where

import Lib

import Graphics.Rendering.Cairo
import qualified Data.Vector as V
import Linear.V2
import Data.RVar
import Data.Random.Distribution.Uniform
import Data.Random.Distribution.Normal
import Data.Random.Source.PureMT


import Data.Foldable
import Control.Monad.State
import Control.Monad.Reader

runGenerate :: World -> PureMT -> Generate a -> a
runGenerate world rng scene = 
  runReader (fmap fst $ runStateT scene rng) world

sketch :: [(Double, Double)] -> (Generate (Render ()))
sketch offsets =  do
  -- bg
  -- for_ offsets normalFillPixel
  -- for_ [0.1, 0.4, 0.7] square
  let pixels = fmap normalFillPixel offsets
  rs <- sequence $ [bg] <> pixels <> fmap square [0.1, 0.4, 0.7]
  return $ foldr1 (>>) rs

animation :: Int -> State PureMT [(Generate (Render()))] 
animation frames = do
  -- would be nice to pass in the distribution as a parameter
  -- also need the size of the square to properly calibrate the normal distribution
  dxs <- traverse sampleRVar $ take frames $ repeat $ normal 0 0.4
  dys <- traverse sampleRVar $ take frames $ repeat $ normal 0 0.33
  let offsets = zip dxs dys
 
  return $ map (\f -> sketch $ take f offsets) [1 .. frames] 
  -- sequence $ map (\f -> sketch $ take f offsets) [1 .. frames] 
    

writeSketch :: World -> PureMT -> String -> Generate (Render ()) -> IO()
writeSketch world rng path sketch = do
  surface <-
    createImageSurface
      FormatARGB32
      (round $ worldWidth world)
      (round $ worldHeight world)
  renderWith surface $ do
    scale (scaleFactor world) (scaleFactor world)
    runGenerate world rng sketch
  surfaceWriteToPNG surface path

leftPad :: Char -> Int -> String -> String
leftPad c n src = (replicate (n - length src) c) ++ src

main :: IO ()
main = do
  rng <- newPureMT
  let world = World 600 200 1
  let frames = 1000
  let frameRenders = evalState (animation frames) rng
  let filenames = map (\i -> "./out/" <>  (leftPad '0' 4 $ show i) <> ".png") [1 .. frames]
  foldr1 (>>) $
    map (uncurry $ writeSketch world rng) $ zip filenames $ frameRenders



{-
  - create all dots before the animation 
  - when animating, take <frames> from the list of dots
-}