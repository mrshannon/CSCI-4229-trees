{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Graphics.Primitives.Open
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Non-closed primitives.
-------------------------------------------------------------------------------



module Graphics.Primitives.Open
( drawCylinder
, drawCone
, drawConicalFrustum
) where


import Data.Word
import Control.Monad
import Graphics.Types
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.OpenGL.Help as GL
import qualified Graphics.Primitives.Flat as Flat




-- Draw an open cylinder.
drawCylinder :: Shading -> Word -> IO ()

-- For wireframe.
drawCylinder Wire faces = do
        Flat.drawCircle Wire faces
        GL.preservingMatrix $ do
            GL.translate $ GL.vector3f 0.0 0.0 1.0
            Flat.drawCircle Wire faces
        GL.renderPrimitive GL.Lines $ forM_ [0.0,ds..(2*pi - ds)] (\s -> do
                 GL.vertex $ GL.vertex3f (cos s) (sin s) 0.0
                 GL.vertex $ GL.vertex3f (cos s) (sin s) 1.0
            )
    where
        ds = 2 * pi / fromIntegral faces

-- For flat shading.
drawCylinder Flat faces = mapM_ loop sVar
    where
        ds     = 2 * pi / fromIntegral faces
        sVar   = [0.0,ds..(2*pi-ds)]
        loop s = GL.renderPrimitive GL.Quads $ do

            -- Set normal for face.
            GL.normal $ GL.normal3f (cos $ s + 0.5*ds) (sin $ s + 0.5*ds) 0.0

            -- Draw left edge of face.
            GL.texCoord2f (s / (2*pi)) 1.0
            GL.vertex $ GL.vertex3f (cos s) (sin s) 1.0
            GL.texCoord2f (s / (2*pi)) 0.0
            GL.vertex $ GL.vertex3f (cos s) (sin s) 0.0

            -- Draw right edge of face.
            GL.texCoord2f ((s + ds) / (2*pi)) 0.0
            GL.vertex $ GL.vertex3f (cos $ s + ds) (sin $ s + ds) 0.0
            GL.texCoord2f ((s + ds) / (2*pi)) 1.0
            GL.vertex $ GL.vertex3f (cos $ s + ds) (sin $ s + ds) 1.0

-- For smooth shading.
drawCylinder Smooth faces = mapM_ loop sVar
    where
        ds     = 2 * pi / fromIntegral faces
        sVar   = [0.0,ds..(2*pi-ds)]
        loop s = GL.renderPrimitive GL.Quads $ do

            -- Draw left edge of face.
            GL.normal $ GL.normal3f (cos s) (sin s) 1.0
            GL.texCoord2f (s / (2*pi)) 1.0
            GL.vertex $ GL.vertex3f (cos s) (sin s) 1.0
            GL.texCoord2f (s / (2*pi)) 0.0
            GL.vertex $ GL.vertex3f (cos s) (sin s) 0.0

            -- Draw left edge of face.
            GL.normal $ GL.normal3f (cos $ s + ds) (sin $ s + ds) 1.0
            GL.texCoord2f ((s + ds) / (2*pi)) 0.0
            GL.vertex $ GL.vertex3f (cos $ s + ds) (sin $ s + ds) 0.0
            GL.texCoord2f ((s + ds) / (2*pi)) 1.0
            GL.vertex $ GL.vertex3f (cos $ s + ds) (sin $ s + ds) 1.0




-- Draw an open cone.
drawCone :: Shading -> Word -> IO ()

-- For wireframe.
drawCone Wire faces = do
        Flat.drawCircle Wire faces
        GL.renderPrimitive GL.Lines $ forM_ [0.0,ds..(2*pi - ds)] (\s -> do
                 GL.vertex $ GL.vertex3f (cos s) (sin s) 0.0
                 GL.vertex $ GL.vertex3f 0.0 0.0 1.0
            )
    where
        ds = 2 * pi / fromIntegral faces

-- For flat shading.
drawCone Flat faces = mapM_ loop sVar
    where
        ds     = 2 * pi / fromIntegral faces
        sVar   = [0.0,ds..(2*pi-ds)]
        loop s = GL.renderPrimitive GL.Triangles $ do

            -- Set normal for face.
            GL.normal $ GL.normal3f (cos $ s + 0.5*ds) (sin $ s + 0.5*ds) 1.0

            -- Draw top of triangle.
            GL.texCoord2f 0.5 0.5
            GL.vertex $ GL.vertex3f 0.0 0.0 1.0

            -- Draw base of triangle.
            GL.texCoord2f (0.5 + 0.5 * cos s) (0.5 + 0.5 * sin s)
            GL.vertex $ GL.vertex3f (cos s) (sin s) 0.0
            GL.texCoord2f (0.5 + 0.5 * (cos $ s + ds))
                          (0.5 + 0.5 * (sin $ s + ds))
            GL.vertex $ GL.vertex3f (cos $ s + ds) (sin $ s + ds) 0.0

-- For smooth shading.
drawCone Smooth faces = mapM_ loop sVar
    where
        ds     = 2 * pi / fromIntegral faces
        sVar   = [0.0,ds..(2*pi-ds)]
        loop s = GL.renderPrimitive GL.Triangles $ do

            -- Draw top of triangle.
            GL.normal $ GL.normal3f (cos $ s + (0.5*ds)) (sin $ s + (0.5*ds)) 1.0
            GL.texCoord2f 0.5 0.5
            GL.vertex $ GL.vertex3f 0.0 0.0 1.0

            -- Draw base of triangle.
            GL.normal $ GL.normal3f (cos s) (sin s) 1.0
            GL.texCoord2f (0.5 + 0.5 * cos s) (0.5 + 0.5 * sin s)
            GL.vertex $ GL.vertex3f (cos s) (sin s) 0.0
            GL.normal $ GL.normal3f (cos $ s + ds) (sin $ s + ds) 1.0
            GL.texCoord2f (0.5 + 0.5 * (cos $ s + ds))
                          (0.5 + 0.5 * (sin $ s + ds))
            GL.vertex $ GL.vertex3f (cos $ s + ds) (sin $ s + ds) 0.0




-- Draw a conical frustum
drawConicalFrustum :: Shading -> Word -> Float -> IO ()

-- For wire shading.
drawConicalFrustum Wire faces topScale = do
        Flat.drawCircle Wire faces
        GL.preservingMatrix $ do
            GL.translate $ GL.vector3f 0.0 0.0 1.0
            GL.scalef topScale topScale 1.0
            Flat.drawCircle Wire faces
        GL.renderPrimitive GL.Lines $ forM_ [0.0,ds..(2*pi - ds)] (\s -> do
                 GL.vertex $ GL.vertex3f (cos s) (sin s) 0.0
                 GL.vertex $ GL.vertex3f (topScale * cos s)
                                         (topScale * sin s)
                                         1.0
            )
    where
        ds = 2 * pi / fromIntegral faces

-- For flat shading.
drawConicalFrustum Flat faces topScale = mapM_ loop sVar
    where
        ds     = 2 * pi / fromIntegral faces
        sVar   = [0.0,ds..(2*pi-ds)]
        loop s = GL.renderPrimitive GL.Quads $ do

            -- Set normal for face.
            GL.normal $ GL.normal3f (cos $ s + 0.5*ds)
                                    (sin $ s + 0.5*ds)
                                    (1.0 - topScale)

            -- Draw left edge of face.
            GL.vertex $ GL.vertex3f (topScale * cos s) (topScale * sin s) 1.0
            GL.vertex $ GL.vertex3f (cos s) (sin s) 0.0

            -- Draw right edge of face.
            GL.vertex $ GL.vertex3f (cos $ s + ds) (sin $ s + ds) 0.0
            GL.vertex $ GL.vertex3f (topScale * cos (s + ds))
                                    (topScale * sin (s + ds))
                                    1.0

-- For smooth shading.
drawConicalFrustum Smooth faces topScale = mapM_ loop sVar
    where
        ds     = 2 * pi / fromIntegral faces
        sVar   = [0.0,ds..(2*pi-ds)]
        loop s = GL.renderPrimitive GL.Quads $ do

            GL.normal $ GL.normal3f (cos s)
                                    (sin s)
                                    (1.0 - topScale)
            -- Draw left edge of face.
            -- GL.normal $ GL.normal3f (sin s) (cos s) (1.0 - topScale)
            GL.vertex $ GL.vertex3f (topScale * cos s) (topScale * sin s) 1.0
            GL.vertex $ GL.vertex3f (cos s) (sin s) 0.0


            GL.normal $ GL.normal3f (cos $ s + ds)
                                    (sin $ s + ds)
                                    (1.0 - topScale)


            -- Draw right edge of face.
            -- GL.normal $ GL.normal3f (cos $ s + ds)
            --                         (sin $ s + ds)
            --                         (1.0 - topScale)
            GL.vertex $ GL.vertex3f (cos $ s + ds) (sin $ s + ds) 0.0
            GL.vertex $ GL.vertex3f (topScale * cos (s + ds))
                                    (topScale * sin (s + ds))
                                    1.0

