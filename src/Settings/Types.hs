{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Settings.Types
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Settings module types.
-------------------------------------------------------------------------------


module Settings.Types
( Settings(..)
) where


import Graphics.Types




data Settings = Settings
    { shading :: Shading
    , lighting :: Bool
    } deriving(Eq, Show)
