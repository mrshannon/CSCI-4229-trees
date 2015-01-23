{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Logic.Controls.Animate
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Controls module (animate).
-------------------------------------------------------------------------------


module Logic.Controls.Animate
( handleAnimate
) where


import Logic.Animation
import qualified App.Types as A
import qualified World.Types as W
import qualified Time.Types as T
import qualified Input.Keyboard.Types as K




-- Handle animation controls.
handleAnimate :: A.App -> A.App
handleAnimate = handlePausing . handleManualAnimation




-- Handle p key.
handlePausing :: A.App -> A.App
handlePausing app =
    if K.KeyPressed == (K.pKey . K.keys . A.keyboard $ app)
        then let world             = A.world app
                 newState = case W.animationState world of
                    W.Running -> W.Paused
                    W.Paused  -> W.Running
             in app { A.world = world { W.animationState = newState }}
        else app




-- Handle [ and ] keys.
handleManualAnimation :: A.App -> A.App
handleManualAnimation app@(A.App { A.keyboard = (K.Keyboard { K.keys = k }) })
        | K.leftBracketKey k  <= K.KeyDown = animate (-dt * 20) app
        | K.rightBracketKey k <= K.KeyDown = animate ( dt * 20) app
        | otherwise              = app
    where
        dt = T.deltaTime . A.time $ app

