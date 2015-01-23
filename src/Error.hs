{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Error
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Error handling and reporting module.
-------------------------------------------------------------------------------


module Error
( printOpenGLErrors
) where


import System.IO
import qualified Graphics.UI.GLUT as GLUT




-- Print OpenGL errors.
printOpenGLErrors :: IO ()
printOpenGLErrors = GLUT.get GLUT.errors >>= mapM_ (hPrint stderr)

