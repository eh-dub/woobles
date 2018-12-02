{-# LANGUAGE RecordWildCards  #-}

module Main where

import Graphics.Rendering.Cairo
import qualified Data.Vector as V
import Linear.V2
import Data.RVar
import Data.Random.Distribution.Uniform
import Data.Random.Source.PureMT

import Data.Foldable
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV
import Control.Monad.State
import Control.Monad.Reader

data World = World 
  { worldWidth  :: Double
  , worldHeight :: Double
  , scaleFactor :: Double
  }

type Generate a  = StateT PureMT (Reader World) a

runGenerate :: World -> PureMT -> Generate a -> a
runGenerate world rng scene = 
  (flip runReader world) . (>>= (return . fst)) . (flip runStateT rng) $ scene

hsva :: Double -> Double -> Double -> Double -> Render ()
hsva h s v = setSourceRGBA channelRed channelGreen channelBlue
        where RGB{..} = hsv h s v

eggshell :: Double -> Render ()
eggshell = hsva 71 0.13 0.96

bg :: Generate (Render ())
bg = do
  (World w h _) <- ask
  return $ do
    eggshell 1
    rectangle 0 0 w h
    fill

sketch :: State Double (Generate (Render ()))
sketch = do
  return $ do
    rs <- sequence [bg]
    return $ foldr1 (const) rs

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
