
module Main where

import Lib

import Graphics.Rendering.Cairo
-- import Data.RVar
import Data.Random
import Data.Random.Source.StdGen
import Data.List


import Data.Foldable
import Control.Monad.State

sketch :: [(Double, Double)] -> App ()
sketch offsets =  do
  bg
  for_ offsets normalFillPixel
  for_ [0.1, 0.4, 0.7] square

animation :: [(Double, Double)] -> [App ()]
animation noise = 
  fmap sketch $ inits noise
    
writeSketch :: World -> MyState -> String -> App a -> IO()
writeSketch world myState path theSketch = do
  surface <-
    createImageSurface
      FormatARGB32
      (round $ worldWidth world)
      (round $ worldHeight world)
  _ <- renderWith surface $ do
    scale (scaleFactor world) (scaleFactor world)
    runApp world myState theSketch
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
  for_ (zip [1 .. frames] frameRenders) $ \(f, r) -> do
    let fileName = "./out/" <>  (leftPad '0' 4 $ show f) <> ".png" 
    writeSketch world mystate fileName r
  pure ()



