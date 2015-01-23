{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : World
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- World module.
-------------------------------------------------------------------------------


module World
( World(..)
, AnimationState(..)
, Sun(..)
, defaultWorld
) where


import World.Types




defaultWorld :: World
defaultWorld = World
    { animationState = Running
    , sun = Sun
        { inclination = 45.0 + 90.0
        , elevation   = 10.0
        }
    , sunTexture      = Nothing
    , groundTexture   = Nothing
    , treeBarkTexture = Nothing
    , leaves1Texture  = Nothing
    , leaves2Texture  = Nothing
    , skyboxTextures  = Nothing
    }

