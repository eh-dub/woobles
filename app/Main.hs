
module Main where

import Lib

import Graphics.Rendering.Cairo
import qualified Data.Vector as V
import Linear.V2
-- import Data.RVar
import Data.Random
import Data.Random.Source.StdGen
import Data.Random.Distribution.Uniform
import Data.Random.Distribution.Normal
-- import Data.Random.Source.PureMT
import Data.List


import Data.Foldable
import Control.Monad.State
import Control.Monad.Reader

-- runGenerate :: World -> PureMT -> Generate a -> a
-- runGenerate world rng scene = 
--   runReader (fmap fst $ runStateT scene rng) world

sketch :: [(Double, Double)] -> App ()
sketch offsets =  do
  bg
  for_ offsets normalFillPixel
  for_ [0.1, 0.4, 0.7] square

animation :: [(Double, Double)] -> [App ()]
animation noise = 
  fmap sketch $ inits noise
    
writeSketch :: World -> MyState -> String -> App a -> IO()
writeSketch world state path sketch = do
  surface <-
    createImageSurface
      FormatARGB32
      (round $ worldWidth world)
      (round $ worldHeight world)
  renderWith surface $ do
    scale (scaleFactor world) (scaleFactor world)
    runApp world state sketch
  surfaceWriteToPNG surface path

leftPad :: Char -> Int -> String -> String
leftPad c n src = (replicate (n - length src) c) ++ src

main :: IO ()
main = do
  let world = World 600 200 1
  let mystate = MyState
  let frames = 1000
  src <- newStdGen
  let (xs, src') = runState (replicateM frames (runRVar (normal 0 0.4) StdRandom)) src
  let ys = evalState (replicateM frames (runRVar (normal 0 0.33) StdRandom)) src'
  let noise = zip xs ys

  let frameRenders = animation noise
  let filenames = map (\i -> "./out/" <>  (leftPad '0' 4 $ show i) <> ".png") [1 .. frames]
  for_ (zip [1 .. frames] frameRenders) $ \(f, r) -> do
    let fileName = "./out/" <>  (leftPad '0' 4 $ show f) <> ".png" 
    writeSketch world mystate fileName r
  pure ()




{-
  - create all dots before the animation 
  - when animating, take <frames> from the list of dots
-}