{-# LANGUAGE NoMonomorphismRestriction #-}
module Lib where

import Control.Monad

import Diagrams.Prelude
import Diagrams.Backend.Cairo

import Data.Colour.Palette.Types
import Data.Colour.Palette.RandomColor
import Data.Random
import Data.Random.Distribution.Categorical

data Wobble = Wobble
  { wFrequency :: Double
  , wMagnitude :: Double
  , wPhase     :: Double
  } deriving (Eq, Ord, Show)

wobblyCircle :: (Double, Double) -> Double -> Wobble -> Colour Double -> Diagram B
wobblyCircle (cx, cy) r (Wobble f m p) color =
  let vertices = (flip fmap) [0, 0.5 .. 360] $ p2 . \d ->
                    let
                      rads = d * (pi / 180)
                      dx = r * cos (rads)
                      dy = r * sin (rads)
                      w = m*cos(f*rads + p)
                      wobbleX = w * cos (rads)
                      wobbleY = w * sin (rads)
                      x = cx + dx + wobbleX
                      y = cy + dy + wobbleY
                    in
                      (x, y)
  in
    fromVertices vertices # glueLine # strokeLoop # lw none # fc color

wooble :: Double -> Wobble -> Colour Double -> Diagram B
wooble r w c = wobblyCircle (0, 0) r w c

myColors :: Int -> IO [Kolor]
myColors numWoobles = do
  brightColors <- replicateM numWoobles $ randomColor HueRandom LumBright
  lightColors  <- replicateM numWoobles $ randomColor HuePurple LumLight
  darkColors   <- replicateM numWoobles $ randomColor HuePurple LumDark
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
