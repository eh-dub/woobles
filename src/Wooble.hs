module Wooble
  (
    wooble
  , wooble'
  , myColors
  , theWoobles
  ) where

import Wooble.Core

import Data.Random
import Data.Random.Source.StdGen
import Data.Coerce

import Control.Monad.State
import Control.Applicative

import Diagrams.Backend.Cairo
import Diagrams.Prelude
import Data.Colour.Palette.Types

theWoobles :: Hue -> Hue -> Hue -> State StdGen (Diagram B)
theWoobles bright light' dark = do
  let radii      = [0.1 :: Double, 0.45 .. 70]
  let numWoobles = length radii

  freqs  <- (replicateM numWoobles $ runRVar (uniform (4 :: Double) 8) StdRandom)
  phases <- (replicateM numWoobles $ runRVar (uniform ((pi :: Double)/4.0) pi) StdRandom)
  colors <- myColors bright light' dark numWoobles
  let woobles = getZipList $
              (\c r f p -> wooble' r (Wobble f (r/75) p) c)
                <$> coerce colors
                <*> coerce radii
                <*> coerce freqs
                <*> coerce phases
  return $ foldr (\d acc -> center d `atop` acc) mempty woobles
