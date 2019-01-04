module Exposition where

import Lib

import Data.List.Split

import Diagrams.Prelude
import Diagrams.Backend.Cairo

magnitude :: Diagram B
magnitude = woobleVariation wobbles
            where
              phases        = 0
              frequencies   = 5
              magnitudes    = [0.01, 0.05, 0.1, 0.5]
              wobbles       = Wobble
                                <$> pure frequencies
                                <*> magnitudes
                                <*> pure phases

frequency :: Diagram B
frequency = woobleVariation wobbles
            where
              phases        = 0
              frequencies   = [1,2,5,10]
              magnitudes    = 0.1
              wobbles       = Wobble
                                <$> frequencies
                                <*> pure magnitudes
                                <*> pure phases


phase :: Diagram B
phase = woobleVariation wobbles
        where
          phases        = [0, (pi::Double)/2 .. 1.5*pi]
          frequencies   = 5
          magnitudes    = 0.1
          wobbles       = Wobble
                            <$> pure frequencies
                            <*> pure magnitudes
                            <*> phases

woobleVariation ::[Wobble] ->  Diagram B
woobleVariation ws =
  bg white
  . center
  . hsep 1
  . fmap (\(Wobble f m p) -> wooble (0,0) 1 (Wobble f m p) medium white)
  $ ws


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

-- frequency And magnitude And phase
fNmNp :: Diagram B
fNmNp =
  let
    ps = [0, (pi::Double)/2 .. 1.5*pi]
    ms = [0.01, 0.05, 0.1, 0.5]
    fs = [1, 2, 5, 10]
    fmps = (\x y z -> (x,y,z)) <$> ps <*> fs <*> ms
  in
    bg white
    . hsep 5
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


