{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}


module Lib where

import Graphics.Rendering.Cairo
import Data.RVar
import Data.Random.Distribution.Uniform
import Data.Random.Source.PureMT
import Linear.V2

import Color

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans.RWS.Strict (RWST (..))
import Control.Monad.Trans
import Data.Foldable

import Debug.Trace
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


bg :: App ()
bg = do
  (World w h _) <- ask
  liftApp $ do
    eggshell 1
    rectangle 0 0 w h
    fill


-- by the time I've set up this function for one concrete config,
-- I should be able to right-click -> design gallery

wobbleApproxCircle :: (Double, Double) -> Double -> Double -> Double -> Double ->  App() 
wobbleApproxCircle (cx, cy) radius degrees f m = do
  (World w h _ ) <- ask
  let startX = cx + radius
  let startY = cy
  liftApp $ do
    newPath
    moveTo startX startY
    for_ [0 .. degrees] $ \degree -> do
      setLineWidth 3
      if (even $ round radius) then do
        englishVermillion 0.5
      else  do
        darkGunmetal 1
      -- f(t) = R + Wcost(F*t)
      -- let wobbleMagnitude = 0.5
      -- let f = 6
      -- let wobbleX =  trace ("mag: " ++ (show w) ++ "      freq: " ++ (show f)) $ w * cos (f*degree * (pi / 180))
      let wobbleX = m * cos (f*degree * (pi / 180))
      let wobbleY = m * sin (f*degree * (pi / 180))

      -- trace  ("dx: " ++ (show $ radius * cos (degree * (pi / 180))) )
      let dx = radius * cos (degree * (pi / 180))
      let dy = radius * sin (degree * (pi / 180))

      -- let x = trace ("x = " ++ show (cx + dx)) $ (cx + dx + wobbleX)
      -- let y = trace ("y = " ++ show (cy + dy)) $ (cy - dy - wobbleY)
      let x =  (cx + dx + wobbleX)
      let y =  (cy - dy - wobbleY)
      lineTo x y
    stroke
    
      


-- noiseMask :: App()
-- noiseMask = do
--   (World w h _) <- ask
--   liftApp $ do
--     for_ [0 .. w*h] $ \n -> do
--       newPath
--       hsva 90 90 90 1
--       let x = abs (w * tan (n + 2389))
--       let y = abs (h * tan (n + 7103))
--       rectangle x y 50 50
--       fill
      -- rectangle 50 50 50 50
      -- fill
      -- where
      --   x = (tan (n + 2389) * w)
      --   y = (tan (n + 7103) * h) 

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

-- Pixel Fills
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