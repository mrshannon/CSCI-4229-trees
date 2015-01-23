{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Main
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- A simple planet genrator and viewer.
-------------------------------------------------------------------------------


module Main where


import Prelude hiding(init)


import System.Random
import Data.Word
import Unsafe.Coerce

import qualified App.GLUT as GLUT

import Math.Vector.Types
import qualified App as A
import qualified Input.Keyboard as K
import qualified Input.Mouse as M
import qualified Window as W
import qualified View as V
import qualified Settings as S
import qualified World as WL
import qualified Time as T

import qualified Graphics.UI.GLUT as GLUT
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL(($=))
import qualified Graphics.Rendering.OpenGL.Draw as GL
import qualified Math.OpenGL as GL
import qualified Graphics.Types as G
import qualified Graphics.Skybox as Sky


import Logic.Controls
import Logic.Animation
import qualified Graphics.Objects.PineTree as PineTree
import qualified Graphics.Primitives.Flat as Flat
import qualified Graphics.Lighting as L
import qualified Graphics.Texture as T
import qualified Graphics.OpenGL.Help as GL
import Graphics.Status
import Error


-- | Compose application parts and run.
main :: IO ()
main = do
        appRef <- A.makeRef app
        GLUT.runApp appRef
    where
        app = A.App
            { A.keyboard  = K.defaultKeyboard
            , A.mouse     = M.defaultMouseClickOnly
            , A.window    = W.Window "Planetoid" 1024 720
            , A.view      = view
            , A.time      = T.defaultTime
            , A.settings  = S.defaultSettings
            , A.world     = WL.defaultWorld
            , A.initFun   = init
            , A.updateFun = update
            , A.drawFun   = draw
            }
        view = V.toThirdPerson V.Orthographic
            { V.sceneCenter = Vector3 (-30.0) (-21.0) 5.0
            , V.sceneSize   = 30.0
            , V.zFactor     = 10.0
            , V.azimuth     = 193.0
            , V.elevation   = 8.0
            , V.tilt        = 0.0
            }




init :: A.App -> IO A.App
init app@(A.App {A.world = w}) = do
    st <- T.loadTexture2D "assets/sun_256x256.jpg"
    gt <- T.loadTexture2D "assets/snow_512x512.jpg"
    bt <- T.loadTexture2D "assets/bark_256x256.jpg"
    l1t <- T.loadTexture2D "assets/pineleaves1_512x512.jpg"
    l2t <- T.loadTexture2D "assets/pineleaves2_512x512.jpg"
    sky <- Sky.loadSkybox
    return app {A.world = w { WL.sunTexture      = Just st
                            , WL.groundTexture   = Just gt 
                            , WL.treeBarkTexture = Just bt
                            , WL.leaves1Texture  = Just l1t
                            , WL.leaves2Texture  = Just l2t
                            , WL.skyboxTextures  = Just sky
                            } }




update :: A.App -> A.App
update = handleSwitchView . handleSwitchShading. rotate . zoom . 
         handleMove . handleAnimation . handleAnimate




draw :: A.App -> IO ()
draw app = do

    let shading         = S.shading . A.settings $ app
        view            = A.view app
        sun             = WL.sun             . A.world $ app
        sunTexture      = WL.sunTexture      . A.world $ app
        groundTexture   = WL.groundTexture   . A.world $ app
        treeBarkTexture = WL.treeBarkTexture . A.world $ app
        leaves1Texture  = WL.leaves1Texture  . A.world $ app
        leaves2Texture  = WL.leaves2Texture  . A.world $ app
        skybox          = WL.skyboxTextures  . A.world $ app
        lightLevel      = max 0.05 (realToFrac $ 0.5 + sin ((WL.elevation sun) * pi/180))

    -- Draw the skybox.
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    GL.color $ GL.color3f lightLevel lightLevel lightLevel
    GL.preservingMatrix $ do
        GL.rotate 90 $ GL.vector3f 0.0 0.0 1.0
        GL.scalef 500.0 500.0 500.0
        case skybox of
            Nothing -> return ()
            Just sky -> Sky.drawSkybox sky

    -- Prepare OpenGL
    GL.depthFunc $= Just GL.Lequal
    GL.cullFace  $= Just GL.Back
    GL.clear [GL.DepthBuffer]

    -- Print status and coordinate frame.
    GL.color (GL.Color3 0.8 0.2 0.0 :: GL.Color3 GL.GLfloat)
    printStatus app
    GL.preservingMatrix $ do
        GL.translate . GL.toVector3d . V.sceneCenter $ view
        GL.drawAxes

    -- Draw the light.
    L.applyLighting app
    GL.textureBinding GL.Texture2D $= sunTexture
    L.drawSun shading sun
    GL.textureBinding GL.Texture2D $= Nothing

    -- Draw ground.
    GL.textureBinding GL.Texture2D $= groundTexture
    GL.materialShininess  GL.Front $= 64.0
    GL.materialSpecular   GL.Front $= GL.Color4 0.5 0.5 1.0 1.0
    GL.materialEmission   GL.Front $= GL.Color4 0.0 0.0 0.0 1.0
    GL.color $ GL.color3f 1.0 1.0 1.0
    GL.preservingMatrix $ do
        GL.scalef 100.0 100.0 1.0
        Flat.drawQuadT shading 40 40 6.0
    GL.textureBinding GL.Texture2D $= Nothing

    -- Draw pine trees.
    let pineTreeTex = PineTree.Textures treeBarkTexture leaves1Texture leaves2Texture
    drawTrees shading pineTreeTex $ take 2000 (randomTrees (mkStdGen 1))

    -- Finish the OpenGL calls.
    GL.flush
    GLUT.swapBuffers
    printOpenGLErrors






data Tree = Tree
    { position    :: (Float, Float)
    , segments    :: Word
    , sections    :: Word
    , rotation    :: Float
    , colorScale  :: Float
    , widthScale  :: Float
    , heightScale :: Float
    }




randomTrees :: StdGen -> [Tree]
randomTrees r0 = tree : randomTrees r8
    where
        (x  , r1) = randomR (-100,  100) r0
        (y  , r2) = randomR (-100,  100) r1
        (seg, r3) = randomR (4  ,  10) r2
        (sec, r4) = randomR (1  ,   6) r3
        (rot, r5) = randomR (0  , 360) r4
        (cs , r6) = randomR (0.4, 1.2) r5
        (w  , r7) = randomR (0.8, 1.2) r6
        (h  , r8) = randomR (0.8, 1.2) r7
        tree = Tree
            { position    = (x, y)
            , segments    = seg
            , sections    = sec
            , rotation    = rot
            , colorScale  = cs
            , widthScale  = w
            , heightScale = h
            }




drawTree :: G.Shading -> PineTree.Textures -> Tree -> IO ()
drawTree shading tex tree = GL.preservingMatrix $ do
        GL.translate $ GL.vector3f x y 0.0
        GL.rotate rot $ GL.vector3f 0.0 0.0 1.0
        GL.scalef w w h 
        PineTree.draw shading tex cs seg sec
    where
        (x, y) = position tree
        seg    = segments tree
        sec    = sections tree
        rot    = unsafeCoerce . rotation $ tree
        cs     = colorScale tree
        w      = widthScale tree
        h      = heightScale tree




drawTrees :: G.Shading -> PineTree.Textures -> [Tree] -> IO ()
drawTrees shading tex = mapM_ (drawTree shading tex)

