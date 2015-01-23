{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Window.GLUT
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Window module (GLUT adapter).
-------------------------------------------------------------------------------


module Window.GLUT
( reshape
) where



import Data.IORef
import qualified Graphics.UI.GLUT as GLUT
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL(($=))
import Window.Types




-- type ReshapeCallback = Size -> IO ()

reshape :: IORef Window -> GLUT.ReshapeCallback
reshape winRef s@(GLUT.Size w h) = do
    GL.viewport $= (GL.Position 0 0, s)
    modifyIORef winRef (\win -> win { width = fromIntegral w
                                    , height = fromIntegral h })
    GLUT.postRedisplay Nothing
    
    
