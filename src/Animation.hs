module Animation where

import Types
import Lib

import Debug.Trace
import Data.Foldable

-- this function is something I want to keep a bunch of different versions of
-- each one represents a different project within a given environment
-- and for each sketch the paramters to it represent an exploration
-- sketch :: (Double, Double) -> [(Double, Wobble)] -> App ()
-- sketch origin circleData =  do
--   bg
--   for_ circleData $ \(r, wobble) -> wobbleApproxCircle origin r 360 wobble
  -- trace (show circleData) $ for_ circleData $ \(r, wobble) -> wobbleApproxCircle (175,125) r 360 wobble

-- draws the animation
-- how make clear the difference between drawing animation and
-- working over the data that the animation is drawn from
-- drawAnimation :: (Double, Double) -> [[(Double, Wobble)]] -> [App ()]
-- drawAnimation origin circles = 
--   fmap (sketch origin) circles

-- transform that is only dependent on time
-- could do this inside the App() type...
-- animate :: Int -> a -> (Int -> a -> b) -> [b]
-- animate frames data' transform =
--   fmap (\x -> transform x data') $ [0 .. frames]

-- produceAnimation :: [App ()]


-- writeAnimations :: World -> MyState -> [[App a]] -> String -> IO()
-- writeAnimations world state animations dest =
--   for_ (zip [1 .. length animations] animations) $ \(i, a) -> do
--     let subFolder = traceShowId $ dest ++ "/" ++ show i ++ "/"
--     createDirectoryIfMissing True subFolder 
--     writeAnimation world state a subFolder

-- writeAnimation :: World -> MyState -> [App a] -> String  -> IO()
-- writeAnimation world state animation dest =
--   for_ (zip [1 .. length animation] animation) $ \(i, f) -> do
--     let fileName = (leftPad '0' 4 $ show i) <> ".png" 
--     let path = dest <> fileName
--     writeSketch world state path f