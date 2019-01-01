
module Main where

import Types
import Lib
import Colors

import Debug.Trace

import Data.Random
import Data.Random.Source.StdGen
import Data.Colour.Palette.RandomColor

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
  seed <- round . (*1000) <$> getPOSIXTime
  let rsrc = mkStdGen seed

  let radii  = [1 :: Double, 1.25 .. 23]
  let numCircs = length radii
  let (freqs, rsrc')  = runState (replicateM numCircs $ runRVar (uniform (15 :: Double) 25) StdRandom) rsrc
  colors <- fmap (take 10) $ do
    randomCIELabPalette
  let selectedColors = evalState (replicateM (length radii) $ runRVar (randomElement colors) StdRandom) rsrc'

  let circles = flip fmap (zip3 selectedColors radii freqs) $ \(c,r,f) -> woobly r (f, r/75) c
  let diagram = foldr (\d acc -> d `atop` acc) mempty circles

  renderCairo "out/test.png" (dims $ V2 300 300) (diagram # bg black)

  pure ()


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
