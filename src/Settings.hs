{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Settings
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Settings module.
-------------------------------------------------------------------------------


module Settings
( Settings(..)
, defaultSettings
) where


import Graphics.Types
import Settings.Types




defaultSettings :: Settings
defaultSettings = Settings
    { shading  = Smooth
    , lighting = True
    }

