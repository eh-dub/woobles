module Exposition where

import Lib

import Diagrams.Prelude
import Diagrams.Backend.Cairo

myBiggerCircle :: Diagram B
myBiggerCircle = circle 2

mySpiral :: Double -> Diagram B
mySpiral r =
  let vertices = (flip fmap) [0, 0.00872664626 .. 2*pi] $ p2 . \rads ->
                    let
                      rx = r * cos(rads)
                      ry = r * sin(rads)
                      radx = rads * cos(rads)
                      rady = rads * sin(rads)
                      x = rx + radx
                      y = ry + rady
                    in
                      (x, y)
  in
    fromVertices vertices

figure1 :: Diagram B
figure1 =  comparison unitCircle myBiggerCircle 8

figure2 :: Diagram B
figure2 = comparison unitCircle (mySpiral 1) 18

figure3a :: Diagram B
figure3a = comparison unitCircle (wooble (0,0) 1 (Wobble 1 1 0) medium white) 6

figure3b :: Diagram B
figure3b = comparison unitCircle (wooble (0,0) 1 (Wobble 5 0.05 0) medium white) 6

figure4 :: Diagram B
figure4 = (center $ hsep 1 [wooble (0,0) 1 (Wobble 5 0.05 0) medium white
                 ,wooble (0,0) 1 (Wobble 9 0.1  0) medium white
                 ,wooble (0,0) 1 (Wobble 3 0.15 0) medium white
                 ])
          `atop`
          background 10

background :: Double -> Diagram B
background bgSize =
  square bgSize # lw none # fc white

comparison :: Diagram B -> Diagram B -> Double -> Diagram B
comparison a b bgSize =
  (center $ hsep 1 [a, b]) `atop` background bgSize


