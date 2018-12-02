
module Main where

import Lib

import Graphics.Rendering.Cairo
import qualified Data.Vector as V
import Linear.V2
import Data.RVar
import Data.Random.Distribution.Uniform
import Data.Random.Source.PureMT

import Data.Foldable
import Control.Monad.State
import Control.Monad.Reader

runGenerate :: World -> PureMT -> Generate a -> a
runGenerate world rng scene = 
  (flip runReader world) . (>>= (return . fst)) . (flip runStateT rng) $ scene

sketch :: State Double (Generate (Render ()))
sketch = do
  return $ do
    rs <- sequence [bg, strokeSquare, fillPixel]
    return $ foldr1 (>>) rs

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

  
main :: IO ()
main = do
  let world = World 500 500 1
  let frames = 100
  rng <- newPureMT
  writeSketch world rng "out.png" $
    do evalState sketch 1
  return ()
