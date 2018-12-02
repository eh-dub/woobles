{-# LANGUAGE RecordWildCards  #-}

module Lib where

import Graphics.Rendering.Cairo
import Data.RVar
import Data.Random.Distribution.Uniform
import Data.Random.Source.PureMT
import Linear.V2


import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV
import Control.Monad.State
import Control.Monad.Reader

data World = World 
  { worldWidth  :: Double
  , worldHeight :: Double
  , scaleFactor :: Double
  }

type Generate a  = StateT PureMT (Reader World) a

hsva :: Double -> Double -> Double -> Double -> Render ()
hsva h s v = setSourceRGBA channelRed channelGreen channelBlue
        where RGB{..} = hsv h s v

eggshell :: Double -> Render ()
eggshell = hsva 71 0.13 0.96

darkGunmetal :: Double -> Render ()
darkGunmetal = hsva 170 0.30 0.16

englishVermillion :: Double -> Render ()
englishVermillion = hsva 355 0.68 0.84

bg :: Generate (Render ())
bg = do
  (World w h _) <- ask
  return $ do
    eggshell 1
    rectangle 0 0 w h
    fill

strokeSquare :: Generate (Render ())
strokeSquare = do
  (World w h _) <- ask
  return $ do
    newPath
    darkGunmetal 1
    rectangle (w/5) (h/5) (3*w/5) (3*h/5)
    stroke 

fillPixel :: (Double, Double) -> Generate (Render ())
fillPixel (dx, dy)= do
  (World w h _) <- ask
  let originX = (w/5) + dx*(3*w/5)
  let originY = (h/5) + dy*(3*h/5)
  return $ do
    englishVermillion 1 
    rectangle originX originY 1 1
    fill