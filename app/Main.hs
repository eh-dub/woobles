module Main where

import Lib

import Data.Random
import Data.Random.Source.StdGen
import Data.Coerce

import Data.Time.Clock.POSIX
import Control.Monad.State
import Control.Applicative

import Linear.V2
import Diagrams.Backend.Cairo
import Diagrams.Size
import Diagrams.Prelude

main :: IO ()
main = do
  seed <- round . (*1000) <$> getPOSIXTime
  let rsrc = mkStdGen seed

  let radii  = [0.1 :: Double, 0.45 .. 70]
  let numWoobles = length radii
  let (freqs, rsrc')  = runState (replicateM numWoobles $ runRVar (uniform (4 :: Double) 8) StdRandom) rsrc
  let phases = evalState (replicateM numWoobles $ runRVar (uniform ((pi :: Double)/4.0) pi) StdRandom) rsrc'

  colors <- myColors numWoobles
  let woobleData = getZipList $
              (\c r f p -> wooble r (Wobble f (r/75) p) c)
                <$> coerce colors
                <*> coerce radii
                <*> coerce freqs
                <*> coerce phases
  let diagram = foldr (\d acc -> center d `atop` acc) mempty woobleData
  let curatedRegion = rectEnvelope (p2 (-48, -40)) (r2 (40, 40))

  renderCairo "out/test.png" (dims $ V2 1600 900) (diagram # bg black # curatedRegion)

  pure ()


{-
  - Make: recreate pieces you've seen
  - Think: original ideas you have for pieces
  - Observe: the process. what kind of interaction design opportunities can be found here.
-}
