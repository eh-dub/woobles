
module Main where

import Types
import Lib
import Colors

import Debug.Trace

import Data.Random
import Data.Random.Distribution.Categorical
import Data.Random.Source.StdGen
import Data.Colour.Palette.Types
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

main :: IO ()
main = do
  seed <- round . (*1000) <$> getPOSIXTime
  let rsrc = mkStdGen seed

  let radii  = [0.1 :: Double, 0.3 .. 50]
  let numCircs = length radii
  let (freqs, rsrc')  = runState (replicateM numCircs $ runRVar (uniform (15 :: Double) 25) StdRandom) rsrc

  brightColors <- replicateM numCircs $ randomColor HueRandom LumBright
  lightColors  <- replicateM numCircs $ randomColor HueRandom LumLight
  darkColors   <- replicateM numCircs $ randomColor HueRandom LumDark
  myColors <- do
    let brightProb = 0.1 :: Double
    flip runRVar StdRandom
     . traverse (\x -> do
          let p = (1 - brightProb)*x
          group <- weightedCategorical
                    [ (p, darkColors)
                    , (1 - brightProb - p, lightColors)
                    , (brightProb, brightColors)
                    ]
          randomElement group)
      . fmap (/ fromIntegral numCircs)
      $ [0 .. fromIntegral numCircs]

  let circles = flip fmap (zip3 myColors radii freqs) $ \(c,r,f) -> woobly r (f, r/75) c
  let diagram = foldr (\d acc -> d `atop` acc) mempty circles

  renderCairo "out/test.png" (dims $ V2 600 600) (diagram # bg black)

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
