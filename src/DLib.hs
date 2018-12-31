{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
module DLib where

import Diagrams.Prelude
import Diagrams.Attributes
import Diagrams.TwoD
import Diagrams.TwoD.Arrow
import Diagrams.Trail
import Diagrams.Backend.Cairo

import MyMonadState
import Data.Foldable

import Types

eggshell :: Colour Double
eggshell = sRGB24 240 234 214

darkGunmetal :: Colour Double
darkGunmetal = sRGB24 29 41 39

myCircle :: Double -> Diagram B
myCircle r = 
  circle r

addLayer :: Diagram B -> DApp () 
addLayer layer = do
  diagram <- get
  put $ layer `atop` diagram

wobblyCircle :: (Double, Double) -> Double -> Wobble -> Diagram B
wobblyCircle (cx, cy) r (f, m) =
  let vertices = (flip fmap) [0, 0.5 .. 360] $ p2 . \d ->
                    let 
                      dx = r * cos (d * (pi / 180))
                      dy = r * sin (d * (pi / 180))
                      w = m*cos(f*d * (pi / 180))
                      wobbleX = w * cos (d * (pi / 180))
                      wobbleY = w * sin (d * (pi / 180))
                      x = cx + dx + wobbleX
                      y = cy + dy + wobbleY
                    in
                      (x, y)
  in 
    fromVertices vertices # strokeLine # showOrigin # lc darkGunmetal
  -- put example

mySketch :: [Wobble] -> DApp ()
mySketch wobbles = do
  -- for_ [1:: Double, 2, 3, 5, 8, 13, 21] (addLayer . myCircle)
  addLayer $ square 40 # fc eggshell # showOrigin
  for_ wobbles $ \w -> addLayer $ translateX 13 $ wobblyCircle (0, 0) 10 w
  -- addLayer $ translateX 13 $ wobblyCircle (0, 0) 10 (0.2, 0.5)
