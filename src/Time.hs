{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Time
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Time module.
-------------------------------------------------------------------------------


module Time
( Time(..)
, defaultTime
, updateTime
) where


import Time.Types



defaultTime :: Time
defaultTime = Time
    { currentTime = 0.0
    , deltaTime   = 0.0
    }




updateTime :: Time -> Double -> Time
updateTime (Time { currentTime = oldTime }) newTime = Time
    { currentTime = newTime
    , deltaTime   = newTime - oldTime
    }

