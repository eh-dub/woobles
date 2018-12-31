
module Main where

import Types
import Animation
import Lib

import Debug.Trace

import Graphics.Rendering.Cairo
import Data.Random
import Data.Random.Source.StdGen

import Data.Time.Clock.POSIX
import Data.Foldable
import Control.Monad.State
import Control.Applicative
import System.Directory

import Linear.V2
import Diagrams.Backend.Cairo
import Diagrams.Size

leftPad :: Char -> Int -> String -> String
leftPad c n src = (replicate (n - length src) c) ++ src

-- @TODO
--  - remove all non-diagrams drawing code from this branch
--  - animate a circle growing
--  - interactive exploration
main :: IO ()
main = do
  let world = World 300 300 1
  let mystate = MyState
  seed <- round . (*1000) <$> getPOSIXTime

  let frames = 25
  -- START INPUT CREATION
  let rsrc = mkStdGen seed
  let radii = [1 :: Double, 2, 3, 5, 8, 13, 21, 34, 55, 88, 143, 231]
  -- let (wobbles', src') = wobbles 1 src
  -- let (wobbles', src') = wobbles (length radii) src
  -- let circleData = zip radii wobbles'
  -- let (cx, src'')  = runState (runRVar (uniform (0 :: Double) 300) StdRandom) src'
  -- let (cy, _) = runState (runRVar (uniform (0 :: Double) 300) StdRandom) src''
  -- END INPUT CREATION


  let archive = "out/" <> (show seed)
  let latest  = "out/latest"
  -- writeAnimations world mystate animations archive
  -- writeAnimations world mystate animations latest

  let magDist = (uniform (0.1 :: Double) 10)
  let freqDist = (uniform (0.1 :: Double) 0.5)
  let wobbles = evalState (replicateM 10 $ wobble' magDist freqDist) rsrc
  let circleData = zip [1 :: Double, 2 .. 10] wobbles
  let diagram = runDApp world $ mySketch circleData
  renderCairo "out/test.png" (dims $ V2 300 300) diagram

  pure ()

wobble' :: RVar Double -> RVar Double -> State StdGen Wobble
wobble' d1 d2 = liftA2 (,) (runRVar d1 StdRandom) (runRVar d2 StdRandom)

-- What kind of type magic would it take to be able to name these "grow" and "shrink"
growCircles :: Int -> [(Double, Wobble)] -> [(Double, Wobble)]
growCircles by circles =
  (flip fmap) circles $ \(r, (f, m)) -> (r+(5*(fromIntegral by)), (f, m))

shrinkCircles :: Int -> [(Double, Wobble)] -> [(Double, Wobble)]
shrinkCircles by circles =
  (flip fmap) circles $ \(r, (f, m)) -> (r-(5*(fromIntegral by)), (f, m))

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
  - Make: recreate pieces you've seen
  - Think: original ideas you have for pieces
  - Observe: the process. what kind of interaction design opportunities can be found here.
-}
