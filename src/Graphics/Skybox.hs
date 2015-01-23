{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Graphics.Skybox
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Skybox module.
-------------------------------------------------------------------------------


module Graphics.Skybox
( Skybox(..)
, drawSkybox
, loadSkybox
) where


import Graphics.Rendering.OpenGL(($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.OpenGL.Help as GL
import qualified Graphics.Texture as T




data Skybox = Skybox
    { front :: Maybe GL.TextureObject
    , right :: Maybe GL.TextureObject
    , back  :: Maybe GL.TextureObject
    , left  :: Maybe GL.TextureObject
    , top   :: Maybe GL.TextureObject
    } deriving(Show, Eq)




loadSkybox :: IO Skybox
loadSkybox = do
    f <- T.loadTexture2D "assets/iceflow_front.jpg"
    r <- T.loadTexture2D "assets/iceflow_right.jpg"
    b <- T.loadTexture2D "assets/iceflow_back.jpg"
    l <- T.loadTexture2D "assets/iceflow_left.jpg"
    t <- T.loadTexture2D "assets/iceflow_top.jpg"
    return Skybox
        { front = Just f
        , right = Just r
        , back  = Just b
        , left  = Just l
        , top   = Just t
        }



drawSkybox :: Skybox -> IO ()
drawSkybox sky = do

    -- Turn off lighting and turn on textures.
    GL.lighting                    $= GL.Disabled
    GL.texture GL.Texture2D        $= GL.Enabled

    -- Draw front.
    GL.textureBinding GL.Texture2D $= front sky
    GL.preservingMatrix $ do
        GL.rotate (-90.0) $ GL.vector3f 1.0 0.0 0.0
        drawSide

    -- Draw right.
    GL.textureBinding GL.Texture2D $= right sky
    GL.preservingMatrix $ do
        GL.rotate   90.0 $ GL.vector3f 0.0 0.0 1.0
        GL.rotate (-90.0) $ GL.vector3f 1.0 0.0 0.0
        drawSide

    -- Draw back.
    GL.textureBinding GL.Texture2D $= back sky
    GL.preservingMatrix $ do
        GL.rotate  180.0 $ GL.vector3f 0.0 0.0 1.0
        GL.rotate (-90.0) $ GL.vector3f 1.0 0.0 0.0
        drawSide

    -- Draw left.
    GL.textureBinding GL.Texture2D $= left sky
    GL.preservingMatrix $ do
        GL.rotate (-90.0) $ GL.vector3f 0.0 0.0 1.0
        GL.rotate (-90.0) $ GL.vector3f 1.0 0.0 0.0
        drawSide

    -- Draw top.
    GL.textureBinding GL.Texture2D $= top sky
    GL.preservingMatrix $ do
        GL.rotate (-180) $ GL.vector3f 1.0 0.0 0.0
        drawSide

    -- Unbind the texture.
    GL.textureBinding GL.Texture2D $= Nothing
    




drawSide :: IO ()
drawSide = GL.renderPrimitive GL.Quads $ do
     GL.normal $ GL.normal3f 0.0 0.0 1.0
     GL.texCoord2f 0.0 0.0
     GL.vertex $ GL.vertex3f (-1.0) (-1.0) (-0.99)
     GL.texCoord2f 1.0 0.0
     GL.vertex $ GL.vertex3f   1.0  (-1.0) (-0.99)
     GL.texCoord2f 1.0 1.0
     GL.vertex $ GL.vertex3f   1.0    1.0  (-0.99)
     GL.texCoord2f 0.0 1.0
     GL.vertex $ GL.vertex3f (-1.0)   1.0  (-0.99)


