

-------------------------------------------------------------------------------
-- |
-- Module       : App.GLUT
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Main application module (GLUT adapter).
-------------------------------------------------------------------------------


module App.GLUT
( runApp
, startFrameTimer
, display
) where


import App.Types
import Data.Word
import qualified Graphics.UI.GLUT as GLUT
import Graphics.UI.GLUT(($=))
import qualified Time.GLUT as GLUT
import Input.Keyboard(cycleKeyboard)
import Input.Mouse(cycleMouse)
import qualified Window as W
import qualified App as A
import qualified Window.GLUT as GLUT
import qualified Graphics.OpenGL as GL
import qualified Input.Keyboard.GLUT as GLUT




glutDisplayOptions :: [GLUT.DisplayMode]
glutDisplayOptions = [ GLUT.RGBMode
                     , GLUT.DoubleBuffered
                     , GLUT.WithDepthBuffer
                     , GLUT.WithSamplesPerPixel 4
                     ]




runApp :: RefApp -> IO ()
runApp refApp = do
    app <- A.pullRef refApp
    (_, _) <- GLUT.getArgsAndInitialize
    GLUT.initialDisplayMode $= glutDisplayOptions
    GLUT.initialWindowSize $= GLUT.Size 1024 720
    _ <- GLUT.createWindow . W.title . A.window $ app
    registerCallbacks refApp
    initilizedApp <- A.initFun app app
    A.putRef initilizedApp refApp
    GLUT.mainLoop





registerCallbacks :: RefApp -> IO ()
registerCallbacks refApp = do
    startFrameTimer 60
    GLUT.reshapeCallback $= Just (GLUT.reshape (windowRef refApp))
    GLUT.displayCallback $= display refApp
    GLUT.initilizeKeyboard . keyboardRef $ refApp







-- Start the frame timer.
startFrameTimer :: Word -> IO ()
startFrameTimer fps = 
    GLUT.addTimerCallback (1000 `div` fromIntegral fps) frameTimer
    where
        timeout = 1000 `div` fromIntegral fps
        frameTimer = do
            GLUT.postRedisplay Nothing
            GLUT.addTimerCallback timeout frameTimer





display :: A.RefApp -> GLUT.DisplayCallback
display refApp = do 
    app1 <- A.pullRef refApp
    t <- GLUT.updateTime . A.time $ app1
    let app2 = app1 { A.time = t}
        app3 = A.updateFun app2 app2
        kb   = A.keyboard app3
        m    = A.mouse app3
        app4 = app3 { A.keyboard = cycleKeyboard kb, A.mouse = cycleMouse m }
    GL.applyProjection (A.window app4) (A.view app4)
    GL.applyView (A.view app4)
    drawFun app4 app4
    A.putRef app4 refApp

