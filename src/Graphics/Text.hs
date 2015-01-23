{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Graphics.Text
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Graphics text module.
-------------------------------------------------------------------------------


module Graphics.Text
( screenPrint
) where


import qualified Graphics.UI.GLUT as GLUT




-- Font to use in glutPrint.
font :: GLUT.BitmapFont
font = GLUT.Helvetica12


-- Print text to screen.
screenPrint :: String -> IO ()
screenPrint = GLUT.renderString font 

