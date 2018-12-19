
module Main where

import Lib

import Graphics.Rendering.Cairo
import Data.Random
import Data.Random.Source.StdGen
import Data.List

import Debug.Trace

import Data.Foldable
import Control.Monad.State

-- this function is something I want to keep a bunch of different versions of
-- each one represents a different project within a given environment
-- and for each sketch the paramters to it represent an exploration
sketch :: [Double] -> [Double] -> [Double] -> App ()
sketch radii wobbleFs wobbleWs =  do
  bg
  let circleData = zip radii $ zip wobbleFs wobbleWs
  for_ circleData $ \(r, (f, w)) -> wobbleApproxCircle (175,125) r 360 f w

-- I want the option of animation on hand all the time
-- animation :: [(Double, Double)] -> [App ()]
-- animation noise = 
--   fmap sketch $ inits noise
    
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
  let world = World 300 300 1
  let mystate = MyState
  src <- newStdGen

  let frames = 1
  let radii = [1, 2, 3, 5, 8, 13, 21, 34, 55, 88, 143, 231]
  let (wobbleFs, src') = runState (replicateM (length radii) (runRVar (uniform (0.1:: Double) 10) StdRandom)) src
  let wobbleWs = trace (show wobbleFs) $ evalState (replicateM (length radii) (runRVar (uniform (0.1 :: Double) 5) StdRandom)) src'
  -- let noise = zip xs ys
  let frameRenders = [sketch radii wobbleFs wobbleWs]
  -- let frameRenders = animation noise
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
