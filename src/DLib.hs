{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
module DLib where

import Diagrams.Prelude
import Diagrams.Attributes
import Diagrams.TwoD
import Diagrams.TwoD.Arrow
import Diagrams.Trail
import Diagrams.Backend.Cairo

import MyMonadState
import Data.Coerce

import Types

myCircle :: DApp ()
myCircle = do
  put $ circle 1