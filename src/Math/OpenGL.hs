{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Math.OpenGL
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- OpenGL Math.
-------------------------------------------------------------------------------


module Math.OpenGL
( toVector3f
, toVector3d
, toVertex3f
, toVertex3d
, fromVector3f
, fromVector3d
) where


import qualified Graphics.Rendering.OpenGL as GL
import Unsafe.Coerce
import Math.Vector.Types




toVector3f :: Vector3 Float -> GL.Vector3 GL.GLfloat
toVector3f (Vector3 x y z) = 
    GL.Vector3 ( unsafeCoerce x )
               ( unsafeCoerce y )
               ( unsafeCoerce z )




toVector3d :: Vector3 Double -> GL.Vector3 GL.GLdouble
toVector3d (Vector3 x y z) = 
    GL.Vector3 ( unsafeCoerce x )
               ( unsafeCoerce y )
               ( unsafeCoerce z )





toVertex3f :: Vector3 Float -> GL.Vertex3 GL.GLfloat
toVertex3f (Vector3 x y z) = 
    GL.Vertex3 ( unsafeCoerce x )
               ( unsafeCoerce y )
               ( unsafeCoerce z )




toVertex3d :: Vector3 Double -> GL.Vertex3 GL.GLdouble
toVertex3d (Vector3 x y z) = 
    GL.Vertex3 ( unsafeCoerce x )
               ( unsafeCoerce y )
               ( unsafeCoerce z )




fromVector3f :: GL.Vector3 GL.GLfloat -> Vector3 Float
fromVector3f (GL.Vector3 x y z) = 
    Vector3 ( unsafeCoerce x )
            ( unsafeCoerce y )
            ( unsafeCoerce z )




fromVector3d :: GL.Vector3 GL.GLdouble -> Vector3 Double
fromVector3d (GL.Vector3 x y z) = 
    Vector3 ( unsafeCoerce x )
            ( unsafeCoerce y )
            ( unsafeCoerce z )

