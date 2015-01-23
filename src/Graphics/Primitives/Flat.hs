{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Graphics.Primitives.Flat
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- 2D primitives.
-------------------------------------------------------------------------------



module Graphics.Primitives.Flat
( drawCircle
, drawQuad
, drawQuadT
) where


import Data.Word
import Control.Monad
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.OpenGL.Help as GL
import Graphics.Types




-- Draw a cirlce.
drawCircle :: Shading -> Word -> IO ()
drawCircle shading faces
    | shading == Wire = drawCircleWire faces
    | otherwise       = drawCircleSurface shading faces




-- Helper function for drawCircle.
drawCircleWire :: Word -> IO ()
drawCircleWire faces = GL.renderPrimitive GL.LineLoop $ mapM_ loop sVar
    where
        ds     = 2 * pi / fromIntegral faces
        sVar   = [0.0,ds..2*pi]
        loop s = GL.vertex $ GL.vertex2f (cos s) (sin s)




-- Helper function for drawCirlce.
drawCircleSurface :: Shading -> Word -> IO ()
drawCircleSurface _ faces = GL.renderPrimitive GL.TriangleFan $ do

            -- Set normal.
            GL.normal $ GL.normal3f 0.0 0.0 1.0

            -- Draw the center.
            GL.texCoord2f 0.5 0.5
            GL.vertex $ GL.vertex2f 0.0 0.0

            -- Draw the circle.
            mapM_ loop sVar

    where
        ds     = 2 * pi / fromIntegral faces
        sVar   = [0.0,ds..2*pi]
        loop s = do
            GL.texCoord2f (0.5 + 0.5 * (cos $ s + ds))
                          (0.5 + 0.5 * (sin $ s + ds))
            GL.vertex $ GL.vertex2f (cos s) (sin s)




-- Draw a subdivided quad.
drawQuad :: Shading -> Word -> Word -> IO ()
drawQuad shading xSubs ySubs
    | shading == Wire = drawQuadWire xSubs ySubs
    | otherwise       = drawQuadSurface shading xSubs ySubs




-- Draw a subdivided quad (with texture coordinates).
drawQuadT :: Shading -> Word -> Word -> Float -> IO ()
drawQuadT shading xSubs ySubs textureScale
    | shading == Wire = drawQuadWire xSubs ySubs
    | otherwise       = drawQuadSurfaceT shading xSubs ySubs textureScale




-- Helper function for drawQuad and drawQuadT
drawQuadWire :: Word -> Word -> IO ()
drawQuadWire xSubs ySubs = GL.renderPrimitive GL.Lines $ do
            forM_ [-1.0,(-1.0 + dx)..1.0] (\x -> do
                    GL.vertex $ GL.vertex2f x (-1.0)
                    GL.vertex $ GL.vertex2f x   1.0
                )
            forM_ [-1.0,(-1.0 + dy)..1.0] (\y -> do
                    GL.vertex $ GL.vertex2f (-1.0) y
                    GL.vertex $ GL.vertex2f   1.0  y
                )
    where
        dx = 2.0 / fromIntegral xSubs
        dy = 2.0 / fromIntegral ySubs




-- Helper function for drawQuad
drawQuadSurface :: Shading -> Word -> Word -> IO ()
drawQuadSurface _ xSubs ySubs = do

            -- Set normal.
            GL.normal $ GL.normal3f 0.0 0.0 1.0

            -- Draw the quads.
            mapM_ loop coords

    where
        dx         = 2.0 / fromIntegral xSubs
        dy         = 2.0 / fromIntegral ySubs
        xList      = [-1.0,(-1.0 + dx)..(1.0 - dx)]
        yList      = [-1.0,(-1.0 + dy)..(1.0 - dy)]
        coords     = [(x,y) | x <- xList, y <- yList]
        loop (x,y) = GL.renderPrimitive GL.Quads $ do
            GL.vertex $ GL.vertex2f  x        y
            GL.vertex $ GL.vertex2f (x + dx)  y
            GL.vertex $ GL.vertex2f (x + dx) (y + dy)
            GL.vertex $ GL.vertex2f  x       (y + dy)




-- Helper function for drawQuadT
drawQuadSurfaceT :: Shading -> Word -> Word -> Float -> IO ()
drawQuadSurfaceT _ xSubs ySubs textureScale = do

            -- Set normal.
            GL.normal $ GL.normal3f 0.0 0.0 1.0

            -- Draw the quads.
            mapM_ loop coords

    where
        dx         = 2.0 / fromIntegral xSubs
        dy         = 2.0 / fromIntegral ySubs
        xList      = [-1.0,(-1.0 + dx)..(1.0 - dx)]
        yList      = [-1.0,(-1.0 + dy)..(1.0 - dy)]
        coords     = [(x,y) | x <- xList, y <- yList]
        setTexCoord x y = GL.texCoord2f (textureScale * 0.5 * (x + 1.0))
                                        (textureScale * 0.5 * (y + 1.0))
        loop (x,y) = GL.renderPrimitive GL.Quads $ do
            setTexCoord              x        y
            GL.vertex $ GL.vertex2f  x        y
            setTexCoord             (x + dx)  y
            GL.vertex $ GL.vertex2f (x + dx)  y
            setTexCoord             (x + dx) (y + dy)
            GL.vertex $ GL.vertex2f (x + dx) (y + dy)
            setTexCoord              x       (y + dy)
            GL.vertex $ GL.vertex2f  x       (y + dy)




