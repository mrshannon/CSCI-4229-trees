{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Graphics.Lighting
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Lighting module.
-------------------------------------------------------------------------------


module Graphics.Lighting
( applyLighting
, drawSun
) where


import Graphics.Rendering.OpenGL(($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.OpenGL.Help as GL
import qualified Graphics.Primitives.Closed as Closed
import qualified App.Types as A
import qualified World.Types as W
import qualified Settings.Types as S
import qualified Graphics.Types as G




applyLighting :: A.App -> IO ()
applyLighting app = if S.lighting . A.settings $ app
        then do

            -- Set the shading model.
            GL.shadeModel $= case shading of
                G.Wire   -> GL.Flat
                G.Flat   -> GL.Flat
                G.Smooth -> GL.Smooth

            -- Turn on lighting.
            GL.normalize             $= GL.Enabled
            GL.lighting              $= GL.Enabled
            GL.colorMaterial         $= Just (GL.Front, GL.AmbientAndDiffuse)
            GL.lightModelLocalViewer $= GL.Disabled
            GL.lightModelAmbient     $= GL.color4f 0.02 0.02 0.02 1.0

        else do

            -- Turn off lighting.
            GL.normalize             $= GL.Disabled
            GL.lighting              $= GL.Disabled
            GL.lightModelLocalViewer $= GL.Disabled

    where
        shading = S.shading . A.settings $ app




drawSun :: G.Shading -> W.Sun -> IO ()
drawSun shading sun = do

            -- Lighting properties.
            GL.light    ( GL.Light 1 ) $= GL.Enabled
            GL.position ( GL.Light 1 ) $= GL.vertex4f (realToFrac x)
                                                      (realToFrac y)
                                                      (realToFrac z)
                                                      1.0
            GL.ambient  ( GL.Light 1 ) $= GL.color4f (0.1*intesity)
                                                     (0.1*intesity)
                                                     (0.1*intesity)
                                                     1.0
            GL.diffuse  ( GL.Light 1 ) $= GL.color4f intesity
                                                     intesity
                                                     (0.8 * intesity)
                                                     1.0

            -- The actual object.
            GL.materialShininess GL.Front $= 1.0
            GL.materialSpecular  GL.Front $= GL.Color4 1.0 1.0 1.0 1.0
            GL.materialEmission  GL.Front $= GL.Color4 1.0 1.0 0.4 1.0
            GL.color $ GL.color3f 1.0 1.0 0.8
            GL.preservingMatrix $ do
                GL.translate $ GL.vector3d x y z
                GL.scalef 20.0 20.0 20.0
                Closed.drawUVSphere shading 128 64

    where
        inclination = W.inclination sun
        elevation   = W.elevation sun
        x           = 1000 * cos (elevation * pi/180) * sin (inclination * pi/180)
        y           = 1000 * sin (elevation * pi/180) * cos (inclination * pi/180)
        z           = 1000 * sin (elevation * pi/180) * sin (inclination * pi/180)
        intesity    = max 0.05 (realToFrac $ 0.5 + sin (elevation * pi/180))
        -- intesity    = realToFrac $ 0.5 + sin (elevation * pi/180)

