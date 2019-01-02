
module Main where

import Types
import Lib

import Data.Random
import Data.Random.Distribution.Categorical
import Data.Random.Source.StdGen
import Data.Colour.Palette.Types
import Data.Colour.Palette.RandomColor
import Data.Coerce

import qualified Numeric.Noise.Perlin as P

import Data.Time.Clock.POSIX
import Control.Monad.State
import Control.Applicative

import Linear.V2
import Diagrams.Backend.Cairo
import Diagrams.Size
import Diagrams.Prelude
import qualified Diagrams.TwoD.Path.Boolean as B

main :: IO ()
main = do
  seed <- round . (*1000) <$> getPOSIXTime
  let rsrc = mkStdGen seed

  let radii  = [0.1 :: Double, 0.45 .. 70]
  let numCircs = length radii
  let (freqs, rsrc')  = runState (replicateM numCircs $ runRVar (uniform (4 :: Double) 8) StdRandom) rsrc
  let phases = evalState (replicateM numCircs $ runRVar (uniform ((pi :: Double)/4.0) pi) StdRandom) rsrc'

  colors <- myColors numCircs
  let circles = getZipList $
              (\c r f p -> wooble r (Wobble f (r/75) p) c)
                <$> coerce colors
                <*> coerce radii
                <*> coerce freqs
                <*> coerce phases
  let diagram = foldr (\d acc -> center d `atop` acc) mempty circles
  -- let clip = unitSquare  # scale (50) # translate ((-25) ^& (-25))
  let envelope = rectEnvelope (p2 (-48, -40)) (r2 (40, 40))

  renderCairo "out/test.png" (dims $ V2 1600 900) (diagram # bg black # envelope)

  pure ()

myColors :: Int -> IO [Kolor]
myColors numCircs = do
  brightColors <- replicateM numCircs $ randomColor HueRandom LumBright
  lightColors  <- replicateM numCircs $ randomColor HuePurple LumLight
  darkColors   <- replicateM numCircs $ randomColor HuePurple LumDark
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

-- wobble' :: RVar Double -> RVar Double -> State StdGen Wobble
-- wobble' d1 d2 = liftA2 (,) (runRVar d1 StdRandom) (runRVar d2 StdRandom)

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
