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
wobblyCircle (cx, cy) r (f, m) color =
  let vertices = (flip fmap) [0, 0.5 .. 360] $ p2 . \d ->
                    let
                      rads = d * (pi / 180)
                      dx = r * cos (rads)
                      dy = r * sin (rads)
                      w = m*cos(f*rads)
                      wobbleX = w * cos (rads)
                      wobbleY = w * sin (rads)
                      x = cx + dx + wobbleX
                      y = cy + dy + wobbleY
                    in
                      (x, y)
  in
    fromVertices vertices # glueLine # strokeLoop # lw veryThin # lc darkGunmetal # fc color
  -- put example
circleData :: (Double, Double) -> Double -> Wobble -> Diagram B
circleData (cx, cy) r (f, m) =
  let
    originT = "origin: " ++ (show cx) ++ "," ++ (show cy)
    radiusT = "radius: " ++ (show r)
    wobbleFT = "wobble frequency: " ++ (show f)
    wobbleMT = "wobble magnitude: " ++ (show m)
  in
    vsep 1 $ fmap text [originT, radiusT, wobbleFT, wobbleMT]

woobly :: Double -> Wobble -> Colour Double -> Diagram B
woobly r w c = wobblyCircle (0, 0) r w c

--mySketch :: [(Double, Wobble)] -> DApp ()
--mySketch circles = do
--  let woobles = flip fmap circles $ \(r, w) ->
--                                      let
--                                        wooble = wobblyCircle (0, 0) r w
--                                        params = circleData (0,0) r w
--                                      in
--                                       wooble === params
  -- for_ [1:: Double, 2, 3, 5, 8, 13, 21] (addLayer . myCircle)
  -- addLayer $ square 40 # fc eggshell # showOrigin


  --let wooblies = hsep 1 $ flip fmap circles $ \(r,w) -> wobblyCircle (0, 0) r w
  --addLayer $ vsep 1 woobles
  -- addLayer $ translateX 13 $ wobblyCircle (0, 0) 10 (0.2, 0.5)
