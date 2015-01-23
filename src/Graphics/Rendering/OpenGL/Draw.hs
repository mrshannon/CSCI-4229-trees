{-# OPTIONS_GHC -O2 -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Graphics.Rendering.OpenGL.Draw
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : MIT
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Shapes and other primitives using OpenGL.
-------------------------------------------------------------------------------


module Graphics.Rendering.OpenGL.Draw
( drawAxes
, drawLabeledAxes
, drawPoint
, drawVector
) where

import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.Help




-- | Draw colored unlabeled 3D unit axes.
drawAxes :: IO ()
drawAxes = do

        -- Axes lines.
        drawVector red   xVector3f
        drawVector green yVector3f
        drawVector blue  zVector3f

        -- Draw point at origin.
        drawPoint 5.0 white originVertex3f




-- | Draw unlabeled 3D unit axes.
drawLabeledAxes :: IO ()
drawLabeledAxes = do

        -- Draw an unlabeled axes.
        drawAxes

        -- Draw labels.
        preservingMatrix $ do
            scale (1.05::GLfloat) (1.05::GLfloat) (1.05::GLfloat)
            rasterPos xVertex3f
            glutPrint "x"
            rasterPos yVertex3f
            glutPrint "y"
            rasterPos zVertex3f
            glutPrint "z"




-- | 
drawPoint :: GLfloat -> Color3f -> Vertex3f -> IO ()
drawPoint p c v = do
        pointSize $= p
        renderPrimitive Points $ do
            color c
            vertex v


drawVector :: Color3f -> Vector3f -> IO ()
drawVector c (Vector3 x y z) =
        renderPrimitive Lines $ do
            color c
            vertex originVertex3f
            vertex $ Vertex3 x y z
