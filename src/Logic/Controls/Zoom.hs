{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Logic.Controls.Zoom
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Controls module (zooming).
-------------------------------------------------------------------------------


module Logic.Controls.Zoom
( zoom
) where


import qualified App.Types as A
import qualified Input.Keyboard.Types as K
-- import qualified Time.Types as T
import qualified View as V




-- Handle zooming (PageUp and PageDown).
zoom :: A.App -> A.App
zoom app@(A.App { A.keyboard = (K.Keyboard { K.keys = k }) })
    | K.pageUpKey   k <= K.KeyDown = zoom' 1.1 app
    | K.pageDownKey k <= K.KeyDown = zoom' 0.9 app
    | otherwise                    = app




-- Helper function for zoom.
zoom' :: Double -> A.App -> A.App
zoom' factor app@(A.App { A.view = v }) = app { A.view = V.zoom factor v }

