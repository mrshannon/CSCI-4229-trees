{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Graphics.Types
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Graphics module types.
-------------------------------------------------------------------------------


module Graphics.Types
( Shading(..)
) where




data Shading = Flat | Smooth | Wire deriving(Eq, Show)

