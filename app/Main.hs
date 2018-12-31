
module Main where

import Types
import Animation

import Debug.Trace

import Graphics.Rendering.Cairo
import Data.Random
import Data.Random.Source.StdGen

import Data.Time.Clock.POSIX
import Data.Foldable
import Control.Monad.State
import System.Directory

import Linear.V2
import Diagrams.Backend.Cairo
import Diagrams.Size
import DLib

-- writeSketch :: World -> MyState -> String -> App a -> IO()
-- writeSketch world myState path theSketch = do
--   surface <-
--     createImageSurface
--       FormatARGB32
--       (round $ worldWidth world)
--       (round $ worldHeight world)
--   _ <- renderWith surface $ do
--     scale (scaleFactor world) (scaleFactor world)
--     runApp world myState theSketch
--   surfaceWriteToPNG surface path

leftPad :: Char -> Int -> String -> String
leftPad c n src = (replicate (n - length src) c) ++ src


-- @TODO 
--  - remove all non-diagrams drawing code from this branch
--  - animate a circle growing and the wobbles being consistent relative to the size
--  - interactive exploration
main :: IO ()
main = do
  let world = World 300 300 1
  let mystate = MyState
  seed <- round . (*1000) <$> getPOSIXTime

  let frames = 25
  -- START INPUT CREATION
  let src = mkStdGen seed
  let radii = [1 :: Double, 2, 3, 5, 8, 13, 21, 34, 55, 88, 143, 231]
  let (wobbles', src') = wobbles (length radii) src
  -- let circleData = zip radii wobbles'
  let (cx, src'')  = runState (runRVar (uniform (0 :: Double) 300) StdRandom) src'
  let (cy, _) = runState (runRVar (uniform (0 :: Double) 300) StdRandom) src''
  let animations = flip fmap [1 .. 12] (\n -> 
                                    let rs = take n radii
                                        circleData = zip rs wobbles'
                                    in
                                    drawAnimation (cx, cy) $ animate frames circleData shrinkCircles 
                                    )


  -- let radii = [1 :: Double, 2, 3, 5, 8, 13, 21, 34, 55, 88, 143, 231]
  -- let (wobbles', src') = wobbles (length radii) src
  -- let circleData = zip radii wobbles'
  -- let (cx, src'')  = runState (runRVar (uniform (0 :: Double) 300) StdRandom) src'
  -- let (cy, _) = runState (runRVar (uniform (0 :: Double) 300) StdRandom) src''
  -- END INPUT CREATION


  let archive = "out/" <> (show seed)
  let latest  = "out/latest"
  -- writeAnimations world mystate animations archive
  -- writeAnimations world mystate animations latest
  -- let sketch = myCircle
  let diagram = runDApp world $ mySketch wobbles'
  renderCairo "out/test.png" (dims $ V2 300 300) diagram

  pure ()

-- writeAnimations :: World -> MyState -> [[App a]] -> String -> IO()
-- writeAnimations world state animations dest =
--   for_ (zip [1 .. length animations] animations) $ \(i, a) -> do
--     let subFolder = traceShowId $ dest ++ "/" ++ show i ++ "/"
--     createDirectoryIfMissing True subFolder 
--     writeAnimation world state a subFolder

-- writeAnimation :: World -> MyState -> [App a] -> String  -> IO()
-- writeAnimation world state animation dest =
--   for_ (zip [1 .. length animation] animation) $ \(i, f) -> do
--     let fileName = (leftPad '0' 4 $ show i) <> ".png" 
--     let path = dest <> fileName
--     writeSketch world state path f

wobbles :: Int -> StdGen -> ([Wobble], StdGen)
wobbles num src =
  (zip frequencies magnitudes, src'')
  where
    (frequencies, src') = runState (replicateM num (runRVar (uniform (0.1:: Double) 10) StdRandom)) src
    (magnitudes, src'') = runState (replicateM num (runRVar (uniform (0.1 :: Double) 0.5) StdRandom)) src'

-- What kind of type magic would it take to be able to name these "grow" and "shrink"
growCircles :: Int -> [(Double, Wobble)] -> [(Double, Wobble)]
growCircles by circles =
  (flip fmap) circles $ \(r, (f, m)) -> (r+(5*(fromIntegral by)), (f, m))

shrinkCircles :: Int -> [(Double, Wobble)] -> [(Double, Wobble)]
shrinkCircles by circles =
  (flip fmap) circles $ \(r, (f, m)) -> (r-(5*(fromIntegral by)), (f, m))  
-- want: a dev version that doesn't require input at run time
-- want: an explore version that can be dynamically parameterized


-- animation idea
-- have wobbly circles grow in size  DONE
-- and have new ones take old place
  -- .... how would you even...
  -- how determine when a new circle needs to be made?
  -- every frame could have a queriable predicate? but then creating new
  --    

-- what is the type of an experiment?

-- I could write out an eff ton of different configurations of this animation
-- then need an interactive application to browse through everything that gets produced.

  -- how make "oowahoowahooowah" come out from the circle?

-- how can we write less code to explore alternatives

{-
    TODO:
    - Want to use diagrams for compositing
      - right now im drawing 1 circle, then 2, ..then 12. There is redundant drawing. If
        I used diagrams then I could draw all circles once and then `atop` them together.
      - Can still make .png's directly through cairo and then lift them into a Diagram
    - figure out how to give the entire image a "papery" feel
      - is there a way to add paper crinkles?
    - interactive application for making/previewing gifs. First thing: starting radiuses of circles
-}

{-
  - Make: recreate pieces you've seen
  - Think: original ideas you have for pieces
  - Observe: the process. what kind of interaction design opportunities can be found here.
-}
