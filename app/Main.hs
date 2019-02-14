module Main where

import Wooble

import Data.Random
import Data.Random.Source.StdGen

import Data.Time.Clock.POSIX
import Control.Monad.State

import Diagrams.Backend.Cairo
import Diagrams.Prelude
import Data.Colour.Palette.Types

main :: IO ()
main = do
  art


art :: IO()
art = do
  seed <- round . (*1000) <$> getPOSIXTime
  let rsrc = mkStdGen seed
  let (diagram, _) = flip runState rsrc $ theWoobles HueGreen HuePurple HuePurple
  let curatedRegion = rectEnvelope (p2 (-48, -40)) (r2 (40, 40))
  renderCairo "out/test.png" (dims $ V2 1600 900) (diagram # bg black # curatedRegion)

