
module Main where

import Types
import Lib
import Colors

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
import Diagrams.Prelude

leftPad :: Char -> Int -> String -> String
leftPad c n src = (replicate (n - length src) c) ++ src

-- @TODO
--  - remove all non-diagrams drawing code from this branch
--  - animate a circle growing
--  - interactive exploration
main :: IO ()
main = do
  --let world = World 300 300 1
  seed <- round . (*1000) <$> getPOSIXTime
  let rsrc = mkStdGen seed

  --let frames = 25
  --let radii = [1 :: Double, 2, 3, 5, 8, 13, 21, 34, 55, 88, 143, 231]
  --let archive = "out/" <> (show seed)
  --let latest  = "out/latest"

  -- let magDist = (uniform (0.1 :: Double) 10)
  -- let freqDist = (uniform (0.1 :: Double) 0.5)
  -- let wobbles = evalState (replicateM 3 $ wobble' magDist freqDist) rsrc
  --let circleData = zip [1 :: Double, 2 .. 10] wobbles

  --let colors = fmap runRVartake 6 $ repeat $ randomElement [red, blue, aqua, brown, pink]
  let colors = [myTeal, myFuchsia, myCobalt, myBeige]
  let radii  = [1 :: Double, 1.5 .. 23]
  let selectedColors = evalState (replicateM (length radii) $ runRVar (randomElement colors) StdRandom) rsrc

  let circles = flip fmap (zip selectedColors radii) $ \(c,r) -> woobly r (20, r/75) c
  let diagram = foldr (\d acc -> d `atop` acc) mempty circles

  renderCairo "out/test.png" (dims $ V2 300 300) diagram

  --let diagram = runDApp world $ mySketch circleData
  --putStrLn "radius: "
  --radiusS <- getLine
  --putStrLn "frequency: "
  --frequencyS <- getLine
  --putStrLn "magnitude: "
  --magnitudeS <- getLine
  --r: 5 f: 20 m: 0.05

  --let r = read radiusS
  --let f = read frequencyS
  --let m = read magnitudeS
  -- magnitude should be no more than 10% of the radius
  -- unclear if frequency should be a function of radius
  --let diagram = woobly r (f, m)
  pure ()

--color' :: RVar (Colour Double) -> State StdGen (Colour Double)
--color' opts = runRVar (randomElement opts) StdRandom

wobble' :: RVar Double -> RVar Double -> State StdGen Wobble
wobble' d1 d2 = liftA2 (,) (runRVar d1 StdRandom) (runRVar d2 StdRandom)

-- animation idea
-- have wobbly circles grow in size  DONE
-- and have new ones take old place
  -- .... how would you even...
  -- how determine when a new circle needs to be made?
  -- every frame could have a queriable predicate? but then creating new
  --

  -- how make "oowahoowahooowah" come out from the circle?

-- how can we write less code to explore alternatives

{-
  - Make: recreate pieces you've seen
  - Think: original ideas you have for pieces
  - Observe: the process. what kind of interaction design opportunities can be found here.
-}
