{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Window.Types
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Window module module types.
-------------------------------------------------------------------------------


module Window.Types
( Window(..)
) where


import Data.Word




data Window = Window
    { title   :: String
    , width   :: Word
    , height  :: Word
    } deriving(Eq, Show)


