{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Time.Types
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Time module types.
-------------------------------------------------------------------------------


module Time.Types
( Time(..)
) where





data Time = Time
    { currentTime :: Double
    , deltaTime   :: Double
    } deriving(Eq, Show)

