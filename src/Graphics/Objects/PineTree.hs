{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Graphics.Objects.PineTree
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Pine tree object.
-------------------------------------------------------------------------------


module Graphics.Objects.PineTree
( Textures(..)
, draw
, drawSmooth
) where


import Data.Word
import Unsafe.Coerce
import Graphics.Types
import Graphics.Rendering.OpenGL(($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.OpenGL.Help as GL
import qualified Graphics.Primitives.Closed as Closed




data Textures = Textures
    { bark   :: Maybe GL.TextureObject
    , leaves1 :: Maybe GL.TextureObject
    , leaves2 :: Maybe GL.TextureObject
    }




-- Draw a pine tree.
draw :: Shading -> Textures -> Float -> Word -> Word -> IO ()
draw shading textures colorScale faces levels = do

        -- Setup material properties (no used because it's a tree).
        GL.materialShininess GL.Front $= 1.0
        GL.materialSpecular  GL.Front $= GL.Color4 1.0 1.0 1.0 1.0
        GL.materialEmission  GL.Front $= GL.Color4 0.0 0.0 0.0 1.0

        -- Set color/texture of the trunk.
        case bark textures of
            Nothing -> GL.color $ GL.color3f 0.35 0.17 0.0
            Just tex -> do
                GL.color $ GL.color3f 0.9 0.7 0.5
                GL.textureBinding GL.Texture2D $= Just tex

        -- Draw the trunk.
        GL.preservingMatrix $ do
            GL.scalef (1.0/4.0 + 0.01 * fromIntegral levels)
                      (1.0/4.0 + 0.01 * fromIntegral levels)
                      (2.0 + 0.2 * fromIntegral levels)
            Closed.drawCylinder shading 8


        -- Draw the green part.
        GL.preservingMatrix $ do
            GL.translate $ GL.vector3f 0.0 0.0 (1.5 + 0.1 * fromIntegral levels)
            GL.scalef (1.5 + 0.2 * fromIntegral levels)
                      (1.5 + 0.2 * fromIntegral levels)
                      (5.0 * 0.95 ^ levels)
            greenPart levels

        -- Remove texture bindings.
        GL.textureBinding GL.Texture2D $= Nothing
    where

        -- Recursively draw the green part.
        greenPart 0 = return ()
        greenPart n 
            | n <= 0    = return ()
            | otherwise = do

                -- Set color/texture of the leaves.
                let texture = if odd n 
                                then leaves1 textures 
                                else leaves2 textures
                case texture of
                    Nothing -> GL.color $ GL.color3f 0.0 0.3 0.0
                    Just tex -> do
                        GL.color . fmap ((unsafeCoerce colorScale) *) $ GL.color3f 1.0 1.0 1.0
                        GL.textureBinding GL.Texture2D $= Just tex

                -- Draw the cone and call next section.
                Closed.drawCone shading faces
                GL.preservingMatrix $ do
                    GL.translate $ GL.vector3f 0.0 0.0 0.6
                    GL.scalef 0.75 0.75 0.75
                    greenPart (n - 1)





-- Draw a pine tree.
drawSmooth :: Shading -> Word -> Word -> IO ()
drawSmooth shading faces levels = do

        -- Setup material properties (no used because it's a tree).
        GL.materialShininess GL.Front $= 1.0
        GL.materialSpecular  GL.Front $= GL.Color4 1.0 1.0 1.0 1.0
        GL.materialEmission  GL.Front $= GL.Color4 0.0 0.0 0.0 1.0

        -- Draw the trunk.
        GL.color $ GL.color3f 0.35 0.17 0.0
        GL.preservingMatrix $ do
            GL.scalef (1.0/4.0 + 0.01 * fromIntegral levels)
                      (1.0/4.0 + 0.01 * fromIntegral levels)
                      (2.0 + 0.2 * fromIntegral levels)
            Closed.drawCylinder shading (4 * faces)

        -- Draw the green part.
        GL.color $ GL.color3f 0.0 0.3 0.0
        GL.preservingMatrix $ do
            GL.translate $ GL.vector3f 0.0 0.0 (1.0 + 0.1 * fromIntegral levels)
            GL.scalef (1.5 + 0.2 * fromIntegral levels)
                      (1.5 + 0.2 * fromIntegral levels)
                      (5.0 * 0.95 ^ levels)
            greenPart levels

    where

        -- Recursively draw the green part.
        greenPart 0 = return ()
        greenPart n 
            | n <= 0    = return ()
            | otherwise = do
                Closed.drawConicalFrustum shading faces 0.1
                -- Closed.drawConicalFrustum shading faces
                GL.preservingMatrix $ do
                    GL.translate $ GL.vector3f 0.0 0.0 0.6
                    GL.scalef 0.75 0.75 0.75
                    greenPart (n - 1)
