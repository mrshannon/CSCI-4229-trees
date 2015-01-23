{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Logic.Animation
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Animation module.
-------------------------------------------------------------------------------


module Logic.Animation
( handleAnimation
, animate
, animateSun
) where


import Data.Fixed
import qualified App.Types as A
import qualified World.Types as W
import qualified Time as T




handleAnimation :: A.App -> A.App
handleAnimation app = case W.animationState . A.world $ app of
        W.Running -> animateSun dt app
        W.Paused  -> app
    where
        dt = T.deltaTime . A.time $ app




animate :: Double -> A.App -> A.App
animate dt = animateSun dt




animateSun :: Double -> A.App -> A.App
animateSun dt app = app { A.world = world { W.sun = newSun } }
    where
        world        = A.world app
        sun          = W.sun world
        elevation    = W.elevation sun
        speed        = if elevation <= 220 || elevation >= 320
                            then 2.0
                            else 20.0
        newElevation = (elevation + speed * dt) `mod'` 360
        newSun       = sun { W.elevation = newElevation }

