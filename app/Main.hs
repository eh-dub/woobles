{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}

module Main where

import Lib
import Orphan

import Data.Random
import Data.Random.Source.StdGen
import Data.Coerce

import Data.Time.Clock.POSIX
import Control.Monad.State
import Control.Applicative

import Linear.V2
-- import Diagrams.Backend.Cairo
import Diagrams.Backend.SVG
import Diagrams.Size
import Diagrams.Prelude
import Data.Colour.Palette.Types

import Web.Suavemente
import Web.Suavemente.Diagrams

main :: IO ()
main = do
  -- art
  seed <- round . (*1000) <$> getPOSIXTime
  let rsrc = mkStdGen seed
  suavemente sendDiagram $ do
    brightHue <- enumDropdown ("Bright Hue"::String) HueBlue
    pure $ art rsrc brightHue
  -- pure ()


art :: StdGen -> Hue -> Diagram B
art rsrc brightHue = do
  let
      diagram       = theWoobles rsrc brightHue HuePurple HuePurple
      curatedRegion = rectEnvelope (p2 (-48, -40)) (r2 (40, 40))
    in
    diagram # curatedRegion # bg black
  -- renderCairo "out/test.png" (dims $ V2 1600 900) (diagram # bg black # curatedRegion)


theWoobles :: StdGen -> Hue -> Hue -> Hue -> Diagram B
theWoobles rsrc bright light' dark =
  let
    radii            = [0.1 :: Double, 0.45 .. 70]
    numWoobles       = length radii
    (freqs, rsrc')   = runState (replicateM numWoobles $ runRVar (uniform (4 :: Double) 8) StdRandom) rsrc
    (phases, rsrc'') = runState (replicateM numWoobles $ runRVar (uniform ((pi :: Double)/4.0) pi) StdRandom) rsrc'
    (colors, _)      = myColors rsrc'' bright light' dark numWoobles
    woobles = getZipList $
              (\c r f p -> wooble' r (Wobble f (r/75) p) c)
                <$> coerce colors
                <*> coerce radii
                <*> coerce freqs
                <*> coerce phases
    in
      foldr (\d acc -> center d `atop` acc) mempty woobles

{-
  - Make: recreate pieces you've seen
  - Think: original ideas you have for pieces
  - Observe: the process. what kind of interaction design opportunities can be found here.
-}
