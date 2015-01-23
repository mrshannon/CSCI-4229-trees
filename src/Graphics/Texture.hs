{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Graphics.Texture
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Texture module.
-------------------------------------------------------------------------------


module Graphics.Texture
( loadTexture2D
) where


import System.IO
import System.Exit
import Graphics.Rendering.OpenGL(($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.GLUtil as GLU




-- Thin wrapper around GLUtil's readTexture function that simply crashes on
-- error.
loadTexture2D :: FilePath -> IO GL.TextureObject
loadTexture2D path = do
    GL.texture GL.Texture2D $= GL.Enabled
    GLU.readTexture path >>= (\x -> case x of

            -- Failed texture.
            (Left message) -> do
                hPutStrLn stderr $ "Failed to load " ++ path
                hPutStrLn stderr message
                exitWith (ExitFailure 1)

            -- Texture loaded successfully.
            (Right tex) -> do
                setupTexture
                return tex
        )




-- Setup a texture based on how 
setupTexture :: IO ()
setupTexture = do 
    (major,_) <- GL.get . GL.majorMinor $ GL.glVersion
    if major < 3
        then GL.textureFilter GL.Texture2D $= 
                ((GL.Linear', Nothing), GL.Linear')
        else do
            GL.textureFilter GL.Texture2D $= 
                ((GL.Linear', Just GL.Linear'), GL.Linear')
            GL.generateMipmap' GL.Texture2D

