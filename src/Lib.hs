{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
module Lib where

import Diagrams.Prelude
import Diagrams.Attributes
import Diagrams.TwoD
import Diagrams.TwoD.Arrow
import Diagrams.Trail
import Diagrams.Backend.Cairo

import qualified Data.Colour as C

import MyMonadState
import Data.Foldable

import Types
import Colors


myCircle :: Double -> Diagram B
myCircle r =
  circle r

addLayer :: Diagram B -> DApp ()
addLayer layer = do
  diagram <- get
  put $ layer `atop` diagram

addToRow :: Diagram B -> DApp ()
addToRow x = do
  diagram <- get
  put $ (diagram ||| x)

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
    fromVertices vertices # glueLine # strokeLoop # lw veryThin # lc darkGunmetal # fc color

wooble :: Double -> Wobble -> Colour Double -> Diagram B
wooble r w c = wobblyCircle (0, 0) r w c

