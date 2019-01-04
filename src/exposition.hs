module Exposition where

import Lib

import Data.List.Split

import Diagrams.Prelude
import Diagrams.Backend.Cairo

magnitude :: Diagram B
magnitude = center (hsep 1 $ fmap (\m -> wooble (0,0) 1 (Wobble 5 m 0) medium white) [0.01, 0.05, 0.1, 0.5]) `atop` background 15

frequency :: Diagram B
frequency = center (hsep 1 $ fmap (\f -> wooble (0,0) 1 (Wobble f 0.1 0) medium white) [1, 2, 5, 10]) `atop` background 15

frequencyAndMagnitude :: Diagram B
frequencyAndMagnitude =
  let
    ms = [0.01, 0.05, 0.1, 0.5]
    fs = [1, 2, 5, 10]
    fms = (,) <$> fs <*> ms
  in
    vsep 2
    . fmap (hsep 2)
    . chunksOf (length fs)
    . fmap (\(f,m) -> wooble (0,0) 1 (Wobble f m 0) medium white)
    $ fms

  {-
I it would be handy if I could use the flexbox model to layout diagrams
      -}
fNmNp :: Diagram B
fNmNp =
  let
    ps = [0, (pi::Double)/2 .. 1.5*pi]
    ms = [0.01, 0.05, 0.1, 0.5]
    fs = [1, 2, 5, 10]
    fmps = (\x y z -> (x,y,z)) <$> ps <*> fs <*> ms
  in
    hsep 5
    . fmap (vsep 2)
    . chunksOf (length ms)
    . fmap (hsep 2)
    . chunksOf (length fs)
    . fmap (\(p,f,m) -> wooble (0,0) 1 (Wobble f m p) medium white)
    $ fmps

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


