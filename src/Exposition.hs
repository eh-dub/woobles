module Exposition where
{- THIS FILE GENERATES DIAGRAMS USED IN THE README.
   THERE IS NOTHING HERE YOU NEED FOR GENERATING WOOBLES
-}

import Wooble.Core

import Data.List.Split
import Data.Time.Clock.POSIX

import Diagrams.Prelude
import Diagrams.Backend.Cairo

import Control.Monad.State
import Control.Applicative
import Data.Coerce
import Data.Random
import Data.Random.Source.StdGen

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

figure3b :: Diagram B
figure3b = bg white
           . hsep 1
           $ [unitCircle, (wooble (0,0) 1 (Wobble 5 0.05 0) medium white)]

woobles' :: (Double -> Wobble) -> Diagram B
woobles' w  =
  foldr (\d acc -> center d `atop` acc) mempty
  . fmap (\r -> wooble (0,0) r (w r) medium white)
  $ [0.1 :: Double, 0.45 .. 10]

wooblesVariation :: [Double -> Wobble] ->  Diagram B
wooblesVariation ws =
  bg white
  . hsep 1
  . fmap woobles'
  $ ws

strokedWoobles :: IO()
strokedWoobles = do
  seed <- round . (*1000) <$> getPOSIXTime
  let rsrc = mkStdGen seed

  let radii  = [0.1 :: Double, 0.45 .. 10]
  let numWoobles = length radii
  let (freqs, rsrc')  = runState (replicateM numWoobles $ runRVar (uniform (4 :: Double) 8) StdRandom) rsrc
  let phases = evalState (replicateM numWoobles $ runRVar (uniform ((pi :: Double)/4.0) pi) StdRandom) rsrc'

  let woobles = getZipList $
              (\r f p -> wooble (0,0) r (Wobble f (r/75) p) medium white)
                <$>  coerce radii
                <*>  coerce freqs
                <*>  coerce phases
  let diagram = foldr (\d acc -> center d `atop` acc) mempty woobles

  renderCairo "out/figures/stroked-woobles.png" (dims $ V2 300 300) (diagram # bg white)

  pure ()
