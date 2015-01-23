{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Graphics.Primitives.Closed
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Closed primitives.
-------------------------------------------------------------------------------


module Graphics.Primitives.Closed
( drawCylinder
, drawCone
, drawConicalFrustum
, drawUVSphere
) where


import Data.Word
import Control.Monad
import Math
import Graphics.Types
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.OpenGL.Help as GL
import qualified Graphics.Primitives.Flat as Flat
import qualified Graphics.Primitives.Open as Open




-- Draw a closed cylinder.
drawCylinder :: Shading -> Word -> IO ()
drawCylinder shading faces = do

    -- Draw the open cylinder.
    Open.drawCylinder shading faces

    when (shading == Smooth || shading == Flat) $ do

        -- Draw the top circle.
        GL.preservingMatrix $ do
            GL.translate $ GL.vector3f 0.0 0.0 1.0
            Flat.drawCircle shading faces

        -- Draw the bottom circle.
        GL.preservingMatrix $ do
            GL.rotate 180.0 $ GL.vector3f 1.0 0.0 0.0
            Flat.drawCircle shading faces




-- Draw a closed cone.
drawCone :: Shading -> Word -> IO ()
drawCone shading faces = do

    -- Draw the open cone.
    Open.drawCone shading faces

    -- Draw the bottom circle.
    when (shading == Smooth || shading == Flat) $
        GL.preservingMatrix $ do
        GL.rotate 180.0 $ GL.vector3f 1.0 0.0 0.0
        Flat.drawCircle shading faces




-- Draw a conical frustum
drawConicalFrustum :: Shading -> Word -> Float -> IO ()
drawConicalFrustum shading faces topScale = do

    -- Draw the conical frustum.
    Open.drawConicalFrustum shading faces topScale

    when (shading == Smooth || shading == Flat) $ do

        -- Draw the top circle.
        GL.preservingMatrix $ do
            GL.translate $ GL.vector3f 0.0 0.0 1.0
            GL.scalef topScale topScale 0.0
            Flat.drawCircle shading faces

        -- Draw the bottom circle.
        GL.preservingMatrix $ do
            GL.rotate 180.0 $ GL.vector3f 1.0 0.0 0.0
            Flat.drawCircle shading faces




-- Draw a UV sphere.
drawUVSphere :: Shading -> Word -> Word -> IO ()

-- For wireframe shading.
drawUVSphere Wire lonBands latBands = do
        mapM_ loopParallels (init latList)
        mapM_ loopMeridians (init lonList)
    where
        lonList = linspace 0.0 (2*pi) (lonBands + 1)
        latList = linspace (-pi / 2) (pi / 2) (latBands + 1)
        loopParallels lat = GL.preservingMatrix $ do
            GL.translate $ GL.vector3f 0.0 0.0 (sin lat)
            GL.scalef (cos lat) (cos lat) 1.0
            Flat.drawCircle Wire lonBands
        loopMeridians lon = GL.preservingMatrix $
            GL.renderPrimitive GL.LineStrip $ forM_ latList (\lat -> 
                    GL.vertex $ GL.vertex3f (cos lat * cos lon)
                                            (cos lat * sin lon)
                                            (sin lat)
                )

-- For flat and smooth shading.
drawUVSphere shading lonBands latBands = mapM_ loop coords
    where
        dLon    = 2 * pi / fromIntegral lonBands
        dLat    = pi / fromIntegral latBands
        lonList = [0.0,dLon..(2*pi - dLon)]
        latList = [(-pi/2),(dLat - pi/2)..(pi/2 - dLat)]
        coords  = [(lon,lat) | lon <- lonList, lat <- latList]

        loop (lon,lat) = GL.renderPrimitive GL.Quads $ do

            -- Set normal.
            when (shading == Flat) $
                GL.normal $ GL.normal3f (cos (lat + dLat/2) * cos (lon + dLon/2))
                                        (cos (lat + dLat/2) * sin (lon + dLon/2))
                                        (sin (lat + dLat))

            -- Point 1.
            when (shading == Smooth) $
                GL.normal $ GL.normal3f (cos lat * cos lon)
                                        (cos lat * sin lon)
                                        (sin lat)
            GL.texCoord2f (lon / (2*pi)) (0.5 + lat / (2*pi))
            GL.vertex $ GL.vertex3f (cos lat * cos lon)
                                    (cos lat * sin lon)
                                    (sin lat)

            -- Point 2.
            when (shading == Smooth) $
                GL.normal $ GL.normal3f (cos lat * cos (lon + dLon))
                                        (cos lat * sin (lon + dLon))
                                        (sin lat)
            GL.texCoord2f ((lon + dLon) / (2*pi)) (0.5 + lat / (2*pi))
            GL.vertex $ GL.vertex3f (cos lat * cos (lon + dLon))
                                    (cos lat * sin (lon + dLon))
                                    (sin lat)

            -- Point 3.
            when (shading == Smooth) $
                GL.normal $ GL.normal3f (cos (lat + dLat) * cos (lon + dLon))
                                        (cos (lat + dLat) * sin (lon + dLon))
                                        (sin (lat + dLat))
            GL.texCoord2f ((lon + dLon) / (2*pi)) (0.5 + (lat + dLat) / (2*pi))
            GL.vertex $ GL.vertex3f (cos (lat + dLat) * cos (lon + dLon))
                                    (cos (lat + dLat) * sin (lon + dLon))
                                    (sin (lat + dLat))

            -- Point 4.
            when (shading == Smooth) $
                GL.normal $ GL.normal3f (cos (lat + dLat) * cos lon)
                                        (cos (lat + dLat) * sin lon)
                                        (sin (lat + dLat))
            GL.texCoord2f (lon / (2*pi)) (0.5 + (lat + dLat) / (2*pi))
            GL.vertex $ GL.vertex3f (cos (lat + dLat) * cos lon)
                                    (cos (lat + dLat) * sin lon)
                                    (sin (lat + dLat))

