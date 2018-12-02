
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

sketch :: (Double, Double) -> State [(Double, Double)] (Generate (Render ()))
sketch (dx, dy) = do
    pixelOffsets <- get
    let newOffsets = pixelOffsets <> [(dx, dy)]
    put newOffsets 
    return $ do
      let pixels = fmap fillPixel newOffsets
      rs <- sequence $ [bg, strokeSquare] <> pixels
      return $ foldr1 (>>) rs

animation :: Int -> State PureMT (State [(Double, Double)] [(Generate (Render()))]) 
animation frames = do
  dx <- sampleRVar $ uniform (0 :: Double) (1 :: Double)  
  dy <- sampleRVar $ uniform (0 :: Double) (1 :: Double)  
  -- let x = (<<=) sampleRVar $ uniform (0 :: Double) (1 :: Double)  
  -- sampleRVar $ uniform (0 :: Double) (1 :: Double) <<= 
  -- let dxs = map (const $ sampleRVar $ uniform (0 :: Double) (1 :: Double)) [1 .. frames]
  -- let dys = map (const $ sampleRVar $ uniform (0 :: Double) (1 :: Double)) [1 .. frames]
  -- let dys = map (const $ sampleRVar $ uniform (0 :: Double) (1 :: Double)) [1 .. frames]
  return $ do
    -- offsets <- zip dxs dys
    -- let offsets = zip dxs dys
    sequence $ map (const $ sketch (dx, dy)) [1 .. frames]

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
  rng <- newPureMT
  let world = World 500 500 1
  let frames = 100
  let frameRenders = evalState (animation frames) rng
  let filenames = map (\i -> "./out/" <>  show i <> ".png") [1 .. frames]
  foldr1 (>>) $
    map (uncurry $ writeSketch world rng) $ zip filenames $ evalState frameRenders []
  -- writeSketch world rng "out.png" $
  --   do evalState (sketch (0,0)) [] 
    -- do evalState sketch rng
  -- return ()
