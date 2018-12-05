{-# LANGUAGE RecordWildCards  #-}

module Color where

import Graphics.Rendering.Cairo
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV

hsva :: Double -> Double -> Double -> Double -> Render ()
hsva h s v = setSourceRGBA channelRed channelGreen channelBlue
        where RGB{..} = hsv h s v

eggshell :: Double -> Render ()
eggshell = hsva 71 0.13 0.96

darkGunmetal :: Double -> Render ()
darkGunmetal = hsva 170 0.30 0.16

englishVermillion :: Double -> Render ()
englishVermillion = hsva 355 0.68 0.84