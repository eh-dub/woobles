{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Lib
import Exposition

import Data.Random
import Data.Random.Source.StdGen
import Data.Coerce

import Data.Time.Clock.POSIX
import Control.Monad.State
import Control.Applicative

-- import Linear.V2
-- import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Backend.CmdLine
import Diagrams.Size
import Diagrams.Prelude hiding ((<>), option)
import Data.Colour.Palette.Types

import Options.Applicative

newtype Artable a = Artable a
data ArtOpts = ArtOpts Int

instance Parseable ArtOpts where
  parser = ArtOpts <$> option auto (long "num-woobles" <> help "set the # of woobles")

instance Mainable (Artable (QDiagram Cairo V2 Double Any)) where
  type MainOpts (Artable (QDiagram Cairo V2 Double Any)) = (MainOpts (QDiagram Cairo V2 Double Any), ArtOpts)
  mainRender (opts, ArtOpts n) (Artable d) = mainRender opts ((if (n > 10) then reflectX else id) d)

main :: IO ()
main = do
  seed <- round . (*1000) <$> getPOSIXTime
  let rsrc = mkStdGen seed
  let diagram = art rsrc
  mainWith (Artable diagram)


art :: StdGen -> Diagram B
art rsrc =
  diagram # curatedRegion # bg black
    where
      diagram = theWoobles rsrc HueGreen HuePurple HuePurple
      curatedRegion = rectEnvelope (p2 (-48, -40)) (r2 (40, 40))

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
