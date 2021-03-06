{-# LANGUAGE NoMonomorphismRestriction #-}
module Wooble.Core where

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Random

import Diagrams.Prelude
import Diagrams.Backend.Cairo

import Data.Colour.Palette.Types
import Data.Colour.Palette.RandomColor
import Data.Random
import Data.Random.Distribution.Categorical
import Data.Random.Source.StdGen

data Wobble = Wobble
  { wFrequency :: Double
  , wMagnitude :: Double
  , wPhase     :: Double
  } deriving (Eq, Ord, Show)

wooble :: (Double, Double) -> Double -> Wobble -> Measure Double -> Colour Double -> Diagram B
wooble (cx, cy) r (Wobble f m p) lineWeight color  =
  let vertices = (flip fmap) [0, 0.5 .. 360] $ p2 . \d ->
                    let
                      rads = d * (pi / 180)
                      rx = r * cos (rads)
                      ry = r * sin (rads)
                      wobble = m*cos(f*rads + p)
                      wobbleX = wobble * cos (rads)
                      wobbleY = wobble * sin (rads)
                      x = cx + rx + wobbleX
                      y = cy + ry + wobbleY
                    in
                      (x, y)
  in
    fromVertices vertices # glueLine # strokeLoop # lw lineWeight # fc color

wooble' :: Double -> Wobble -> Colour Double -> Diagram B
wooble' r w c = wooble (0, 0) r w none c

myColors :: Hue -> Hue -> Hue -> Int -> State StdGen [Kolor]
myColors bright light dark numWoobles = do
  src <- get
  let (brightColors, src') = flip runRand src
                             $ replicateM numWoobles $ randomColor bright LumBright
  let (lightColors, src'') = flip runRand src $ replicateM numWoobles $ randomColor light LumLight
  let (darkColors, src''') = flip runRand src $ replicateM numWoobles $ randomColor dark LumDark
  put src'''

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
    . fmap (/ fromIntegral numWoobles)
    $ [0 .. fromIntegral numWoobles]
