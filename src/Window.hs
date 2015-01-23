{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Graphics.OpenGL
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Window module.
-------------------------------------------------------------------------------


module Window
( Window(..)
, aspectRatio
) where


import Window.Types




-- Calculate aspect ratio of window.
aspectRatio :: Window -> Double
aspectRatio window = 
    let w = width window
        h = height window
    in if h > 0
            then fromIntegral w / fromIntegral h
            else 1

