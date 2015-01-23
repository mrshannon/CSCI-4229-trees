{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Logic.Controls
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Controls module.
-------------------------------------------------------------------------------


module Logic.Controls
( zoom
, rotate
, switchView
, handleSwitchView
, switchShading
, handleSwitchShading
, handleMove
, handleAnimate
) where


import qualified App.Types as A
import qualified Input.Keyboard.Types as K
import qualified View as V
import qualified Settings.Types as S
import qualified Graphics.Types as G

import Logic.Controls.Zoom
import Logic.Controls.Rotate
import Logic.Controls.Move
import Logic.Controls.Animate




-- Switch view modes.
switchView :: A.App -> A.App
switchView app@(A.App { A.view = view@(V.Orthographic {}) }) = 
    app { A.view = V.toThirdPerson view }
switchView app@(A.App { A.view = view@(V.ThirdPerson {}) }) = 
    app { A.view = V.toOrthographic view }
switchView app@(A.App { A.view = (V.FirstPerson {}) }) = app




-- Handle c key.
handleSwitchView :: A.App -> A.App
handleSwitchView app =
    if K.KeyPressed == (K.cKey . K.keys . A.keyboard $ app)
        then switchView app
        else app




-- Switch shading modes.
switchShading :: A.App -> A.App
switchShading app@(A.App { A.settings = settings@(S.Settings {}) }) =
    let newShading = case S.shading settings of
            G.Wire   -> G.Flat
            G.Flat   -> G.Smooth
            G.Smooth -> G.Wire
    in app { A.settings = settings { S.shading = newShading } }




-- Handle v key.
handleSwitchShading :: A.App -> A.App
handleSwitchShading app =
    if K.KeyPressed == (K.vKey . K.keys . A.keyboard $ app)
        then switchShading app
        else app



