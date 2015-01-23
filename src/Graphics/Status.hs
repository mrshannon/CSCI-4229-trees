{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Graphics.Status
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Graphics module status printer.
-------------------------------------------------------------------------------


module Graphics.Status
( printStatus
)
where

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.OpenGL.Help as GL
import qualified App.Types as A
import qualified View.Types as V
import qualified Settings.Types as S
import Graphics.Text
import Graphics.Types


printStatus :: A.App -> IO ()
printStatus app = do
        GL.windowPos $ GL.vertex2f 5.0 5.0
        screenPrint displayString
    where
        displayString = "View: " ++ viewName ++ "    Shading: " ++ shadingName
        viewName = case A.view app of
            V.Orthographic {} -> "Orthographic"
            V.ThirdPerson {}  -> "Perspective"
            _                 -> ""
        shadingName = case S.shading . A.settings $ app of
            Wire   {} -> "Wireframe"
            Flat   {} -> "Flat"
            Smooth {} -> "Smooth"

