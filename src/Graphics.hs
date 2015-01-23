{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Graphics
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Graphics module.
-------------------------------------------------------------------------------


module Graphics
( applyProjection
) where


import Window.Types
import View.Types
import qualified Graphics.OpenGL as GL




applyProjection :: Window -> View -> IO ()
applyProjection = GL.applyProjection

