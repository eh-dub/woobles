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

myCircle :: Double -> Diagram B
myCircle r = 
  circle r

addLayer :: Diagram B -> DApp () 
addLayer layer = do
  diagram <- get
  put $ layer `atop` diagram

wobblyCircle :: (Double, Double) -> Double -> Wobble -> DApp ()
wobblyCircle (cx, cy) r (f, m) = do
  let vertices = (flip fmap) [0 .. 360] $ p2 . \d ->
                    let 
                      dx = r * cos (d * (pi / 180))
                      dy = r * sin (d * (pi / 180))
                      wobbleX = m * cos (f*d * (pi / 180))
                      wobbleY = m * sin (f*d * (pi / 180))
                      x = cx + dx + wobbleX
                      y = cy + dy + wobbleY
                    in
                      (x, y)

  -- let vertices = map p2 $ [(x,y) | x <- [0,0.2 .. 2], y <- [0,1]]
  let example = fromVertices vertices # strokeLine
  put example

mySketch :: DApp ()
mySketch = do
  -- for_ [1:: Double, 2, 3, 5, 8, 13, 21] (addLayer . myCircle)
  wobblyCircle (0, 0) 10 (50, 3)
