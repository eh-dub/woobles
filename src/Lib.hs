{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}


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
import Control.Monad.Trans.RWS.Strict (RWST (..))
import Control.Monad.Trans

data World = World 
  { worldWidth  :: Double
  , worldHeight :: Double
  , scaleFactor :: Double
  }

data MyState = MyState 
newtype App a = App {unApp :: RWST World () MyState Render a}
  deriving (Functor, Applicative, Monad, MonadIO
           ,MonadState MyState, MonadReader World)

runApp :: World -> MyState -> App a -> Render a
runApp world state sketch = do
  (a, _, _)<- runRWST (unApp sketch) world state 
  pure a

liftApp :: Render a -> App a
liftApp = App . lift

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

bg :: App ()
bg = do
  (World w h _) <- ask
  liftApp $ do
    eggshell 1
    rectangle 0 0 w h
    fill

square :: Double -> App ()
square x  = do
  (World w h _) <- ask
  liftApp $ do
    newPath
    darkGunmetal 1
    rectangle (x*w) (h/4) (h/2) (h/2)
    stroke 

strokeSquare :: App ()
strokeSquare = do
  (World w h _) <- ask
  liftApp $ do
    newPath
    darkGunmetal 1
    rectangle (w/5) (h/5) (3*w/5) (3*h/5)
    stroke 

uniformFillPixel :: (Double, Double) -> App ()
uniformFillPixel (dx, dy)= do
  (World w h _) <- ask
  let originX = (w/5) + dx*(3*w/5)
  let originY = (h/5) + dy*(3*h/5)
  liftApp $ do
    englishVermillion 1 
    rectangle originX originY 1 1
    fill

normalFillPixel :: (Double, Double) -> App ()
normalFillPixel (dx, dy) = do
  (World w h _) <- ask
  -- how might I query the state of the square here?
  let halfWidth = (w/2)
  let halfHeight = (h/4)
  let centerX = (w/2)
  let centerY = (h/2)
  liftApp $ do
    englishVermillion 1
    rectangle (centerX + dx*halfWidth) (centerY + dy*halfHeight) 1 1
    fill

{-
  - currently
-}