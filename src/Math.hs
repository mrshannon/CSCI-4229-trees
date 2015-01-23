{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Math
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Math module.
-------------------------------------------------------------------------------


module Math
( linspace
) where


import Data.Word




linspace :: (Fractional a, Enum a) => a -> a -> Word -> [a]
linspace start stop n = [start,(start + delta)..stop]
    where
        delta = (stop - start) / fromIntegral (n - 1)

