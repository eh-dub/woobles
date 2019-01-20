{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric  #-}

module Orphan where

import GHC.Generics

import Data.Colour.Palette.Types
import Data.Aeson (FromJSON)
import Text.Blaze

import Web.Suavemente

deriving instance Enum Hue
deriving instance Generic Hue
deriving instance Bounded Hue
deriving instance FromJSON Hue
instance ToMarkup Hue where
  toMarkup = toMarkup . show
