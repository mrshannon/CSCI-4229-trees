{-# OPTIONS_GHC -O2 -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Graphics.Rendering.OpenGL.Help
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : MIT
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- OpenGL helper functions and types.
-------------------------------------------------------------------------------


module Graphics.Rendering.OpenGL.Help
( Vertex3f
, Vector3f
, Color3f
, zeroVector3f
, xVector3f
, yVector3f
, zVector3f
, originVertex3f
, xVertex3f
, yVertex3f
, zVertex3f
, vectorToVertex3
, vertexToVector3
, black
, white
, red
, green
, blue
, cyan
, magenta
, yellow
, glutPrint
, printErrors
) where

import System.IO
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT.Fonts




-- Convenience types.
type Vertex3f = Vertex3 GLfloat
type Vector3f = Vector3 GLfloat
type Color3f  = Color3 GLfloat




-- | Helpful base vectors.
zeroVector3f    :: Vector3f
zeroVector3f    = Vector3 0.0 0.0 0.0
xVector3f       :: Vector3f
xVector3f       = Vector3 1.0 0.0 0.0
yVector3f       :: Vector3f
yVector3f       = Vector3 0.0 1.0 0.0
zVector3f       :: Vector3f
zVector3f       = Vector3 0.0 0.0 1.0




-- | Helpful vertices.
originVertex3f  :: Vertex3f
originVertex3f  = Vertex3 0.0 0.0 0.0
xVertex3f       :: Vertex3f
xVertex3f       = Vertex3 1.0 0.0 0.0
yVertex3f       :: Vertex3f
yVertex3f       = Vertex3 0.0 1.0 0.0
zVertex3f       :: Vertex3f
zVertex3f       = Vertex3 0.0 0.0 1.0




-- | Converts a Vector3 to a Vertex3
vectorToVertex3 :: Vector3 a -> Vertex3 a
vectorToVertex3 (Vector3 x y z) = Vertex3 x y z




-- | Converts a Vertex3 to a Vector3
vertexToVector3 :: Vertex3 a -> Vector3 a
vertexToVector3 (Vertex3 x y z) = Vector3 x y z




-- | Some basic colors.
black   :: Color3f
black   = Color3 0.0 0.0 0.0 :: Color3f
white   :: Color3f
white   = Color3 1.0 1.0 1.0 :: Color3f
red     :: Color3f
red     = Color3 1.0 0.0 0.0 :: Color3f
green   :: Color3f
green   = Color3 0.0 1.0 0.0 :: Color3f
blue    :: Color3f
blue    = Color3 0.0 0.0 1.0 :: Color3f
cyan    :: Color3f
cyan    = Color3 0.0 1.0 1.0 :: Color3f
magenta :: Color3f
magenta = Color3 1.0 0.0 1.0 :: Color3f
yellow  :: Color3f
yellow  = Color3 1.0 1.0 0.0 :: Color3f




-- Font to use in glutPrint.
font :: BitmapFont
font = Helvetica18

-- GLUT string printer.
glutPrint :: String -> IO ()
glutPrint = renderString font 




-- Print OpenGL errors.
printErrors :: IO ()
printErrors = get errors >>= mapM_ (hPrint stderr)
