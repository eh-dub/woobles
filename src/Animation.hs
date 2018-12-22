module Animation where

import Types
import Lib

import Debug.Trace
import Data.Foldable

-- this function is something I want to keep a bunch of different versions of
-- each one represents a different project within a given environment
-- and for each sketch the paramters to it represent an exploration
sketch :: (Double, Double) -> [(Double, Wobble)] -> App ()
sketch origin circleData =  do
  bg
  for_ circleData $ \(r, wobble) -> wobbleApproxCircle origin r 360 wobble
  -- trace (show circleData) $ for_ circleData $ \(r, wobble) -> wobbleApproxCircle (175,125) r 360 wobble

-- I want the option of animation on hand all the time
animation :: (Double, Double) -> [[(Double, Wobble)]] -> [App ()]
animation origin circles = 
  fmap (sketch origin) circles