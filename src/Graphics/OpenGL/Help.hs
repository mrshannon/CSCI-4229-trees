{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Graphics.OpenGL.Help
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- OpenGL helper functions.
-------------------------------------------------------------------------------


module Graphics.OpenGL.Help
( vertex2f
, vertex2d
, vertex3f
, vertex3d
, vertex4f
, vertex4d
, vector2f
, vector2d
, vector3f
, vector3d
, vector4f
, vector4d
, color3f
, color3d
, color4f
, color4d
, normal3f
, normal3d
, scalef
, scaled
, texCoord2f
, texCoord2d
) where


import Graphics.Rendering.OpenGL
import Unsafe.Coerce




-- Make 2 element vertices
vertex2f :: Float -> Float -> Vertex2 GLfloat
vertex2f x y = Vertex2 (unsafeCoerce x) (unsafeCoerce y)

vertex2d :: Double -> Double -> Vertex2 GLdouble
vertex2d x y = Vertex2 (unsafeCoerce x) (unsafeCoerce y)




-- Make 3 element vertices.
vertex3f :: Float -> Float -> Float -> Vertex3 GLfloat
vertex3f x y z = Vertex3 (unsafeCoerce x) (unsafeCoerce y) (unsafeCoerce z)

vertex3d :: Double -> Double -> Double -> Vertex3 GLdouble
vertex3d x y z = Vertex3 (unsafeCoerce x) (unsafeCoerce y) (unsafeCoerce z)




-- Make 4 element vertices.
vertex4f :: Float -> Float -> Float -> Float -> Vertex4 GLfloat
vertex4f x y z w = Vertex4 (unsafeCoerce x)
                           (unsafeCoerce y)
                           (unsafeCoerce z)
                           (unsafeCoerce w)

vertex4d :: Double -> Double -> Double -> Double -> Vertex4 GLdouble
vertex4d x y z w = Vertex4 (unsafeCoerce x)
                           (unsafeCoerce y)
                           (unsafeCoerce z)
                           (unsafeCoerce w)




-- Make 2 element vectors
vector2f :: Float -> Float -> Vector2 GLfloat
vector2f x y = Vector2 (unsafeCoerce x) (unsafeCoerce y)

vector2d :: Double -> Double -> Vector2 GLdouble
vector2d x y = Vector2 (unsafeCoerce x) (unsafeCoerce y)




-- Make 3 element vectors.
vector3f :: Float -> Float -> Float -> Vector3 GLfloat
vector3f x y z = Vector3 (unsafeCoerce x) (unsafeCoerce y) (unsafeCoerce z)

vector3d :: Double -> Double -> Double -> Vector3 GLdouble
vector3d x y z = Vector3 (unsafeCoerce x) (unsafeCoerce y) (unsafeCoerce z)




-- Make 4 element vectors.
vector4f :: Float -> Float -> Float -> Float -> Vector4 GLfloat
vector4f x y z w = Vector4 (unsafeCoerce x)
                           (unsafeCoerce y)
                           (unsafeCoerce z)
                           (unsafeCoerce w)

vector4d :: Double -> Double -> Double -> Double -> Vector4 GLdouble
vector4d x y z w = Vector4 (unsafeCoerce x)
                           (unsafeCoerce y)
                           (unsafeCoerce z)
                           (unsafeCoerce w)




-- Make 3 element colors.
color3f :: Float -> Float -> Float -> Color3 GLfloat
color3f x y z = Color3 (unsafeCoerce x) (unsafeCoerce y) (unsafeCoerce z)

color3d :: Double -> Double -> Double -> Color3 GLdouble
color3d x y z = Color3 (unsafeCoerce x) (unsafeCoerce y) (unsafeCoerce z)




-- Make 4 element colors.
color4f :: Float -> Float -> Float -> Float -> Color4 GLfloat
color4f x y z w = Color4 (unsafeCoerce x)
                           (unsafeCoerce y)
                           (unsafeCoerce z)
                           (unsafeCoerce w)

color4d :: Double -> Double -> Double -> Double -> Color4 GLdouble
color4d x y z w = Color4 (unsafeCoerce x)
                           (unsafeCoerce y)
                           (unsafeCoerce z)
                           (unsafeCoerce w)




-- Make 3 normals colors.
normal3f :: Float -> Float -> Float -> Normal3 GLfloat
normal3f x y z = Normal3 (unsafeCoerce x) (unsafeCoerce y) (unsafeCoerce z)

normal3d :: Double -> Double -> Double -> Normal3 GLdouble
normal3d x y z = Normal3 (unsafeCoerce x) (unsafeCoerce y) (unsafeCoerce z)





-- Apply scale.
scalef :: Float -> Float -> Float -> IO ()
scalef x y z = scale (unsafeCoerce x :: GLfloat) 
                      (unsafeCoerce y :: GLfloat)
                      (unsafeCoerce z :: GLfloat)

scaled :: Double -> Double -> Double -> IO ()
scaled x y z = scale (unsafeCoerce x :: GLdouble) 
                      (unsafeCoerce y :: GLdouble)
                      (unsafeCoerce z :: GLdouble)



-- Set 2D texture coordinate.
texCoord2f :: Float -> Float -> IO ()
texCoord2f x y = texCoord $ TexCoord2 (unsafeCoerce x :: GLfloat)
                                      (unsafeCoerce y :: GLfloat)

texCoord2d :: Double -> Double -> IO ()
texCoord2d x y = texCoord $ TexCoord2 (unsafeCoerce x :: GLdouble)
                                      (unsafeCoerce y :: GLdouble)
