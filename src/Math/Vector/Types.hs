{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Math.Vector.Types
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Vector types.
-------------------------------------------------------------------------------


module Math.Vector.Types
( Vector3(..)
) where



data Vector3 a = Vector3 a a a deriving(Eq, Show)


instance Num a => Num (Vector3 a) where

    -- (+)
    (Vector3 x1 y1 z1) + (Vector3 x2 y2 z2) =
        Vector3 (x1 + x2) (y1 + y2) (z1 + z2)

    -- (*) element wise multiplication.
    (Vector3 x1 y1 z1) * (Vector3 x2 y2 z2) =
        Vector3 (x1 * x2) (y1 * y2) (z1 * z2)

    -- (-)
    (Vector3 x1 y1 z1) - (Vector3 x2 y2 z2) =
        Vector3 (x1 - x2) (y1 - y2) (z1 - z2)

    -- abs
    abs (Vector3 x y z) = Vector3 (abs x) (abs y) (abs z)

    -- signum
    signum (Vector3 x y z) = Vector3 (signum x) (signum y) (signum z)

    -- signum
    fromInteger n = Vector3 (fromInteger n) (fromInteger n) (fromInteger n)
