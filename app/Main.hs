
module Main where

import Lib

import Graphics.Rendering.Cairo
import Data.Random
import Data.Random.Source.StdGen
import Data.List


import Data.Foldable
import Control.Monad.State


sketch :: [(Double, Double)] -> App ()
sketch offsets =  do
  bg
  wobbleApproxCircle (250,75) 30 90
  -- noiseMask
  -- for_ offsets normalFillPixel
  -- for_ [0.1, 0.4, 0.7] square

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
  let frames = 1
  src <- newStdGen
  let (xs, src') = runState (replicateM frames (runRVar (normal 0 0.4) StdRandom)) src
  let ys = evalState (replicateM frames (runRVar (normal 0 0.33) StdRandom)) src'
  let noise = zip xs ys

  let frameRenders = animation noise
  for_ (zip [1 .. frames] frameRenders) $ \(f, r) -> do
    let fileName = "./out/" <>  (leftPad '0' 4 $ show f) <> ".png" 
    writeSketch world mystate fileName r
  pure ()



{-
    TODO:
    - Want to use diagrams for compositing
    - Can still make .png's directly through cairo and then lift them into a Diagram
    - figure out how to give the entire image a "papery" feel
    - is there a way to add paper crinkles?
    - remaking those circles sounds fun. would be interesting to have the animation that draws it too
      randomize the starting location to add some variety to gifs
      and if the widths varied signifiantly
      and maybe draw some figures with the technique
    - step 1 generate a circle using polar coordinates

-}

{-
  - Make: recreate pieces you've seen
  - Think: original ideas you have for pieces
  - Observe: the process. what kind of interaction design opportunities can be found here.
-}
