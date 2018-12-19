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

wobbleApproxCircle :: (Double, Double) -> Double -> Double ->  App() 
wobbleApproxCircle (cx, cy) radius degrees = do
  (World w h _ ) <- ask
  let startX = cx + radius
  let startY = cy
  liftApp $ do
    newPath
    moveTo startX startY
    for_ [0 .. 360] $ \degree -> do
    -- for_ [0, 45, 90, 135, 180, 225, 270, 315, 360] $ \degree -> do
      englishVermillion 1
      setLineWidth 5
      let dx = trace ("dx: " ++ (show $ radius * cos (degree * (pi / 180))) ) radius * cos (degree * (pi / 180))
      let dy = radius * sin (degree * (pi / 180))
      -- trace _ ("dx: " ++ show dx) 
      let x = trace ("x = " ++ show (cx + dx)) $ (cx + dx)
      let y = trace ("y = " ++ show (cy + dy)) $ (cy + dy)
      -- rectangle x y 10 10
      
      lineTo x y
      -- rectangle 0 0 50 50
      -- fill
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