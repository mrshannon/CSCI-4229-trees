{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Input.Mouse.GLUT
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Mouse input module (GLUT adapter).
-------------------------------------------------------------------------------



module Input.Mouse.GLUT
( Mouse(..)
, Buttons(..)
, ButtonState(..)
, Position(..)
, DeltaPosition(..)
, WheelTicks(..)
, cycleMouse
, defaultMouseClickOnly
, defaultMouseDownOnly
, defaultMouseTracking
, defaultMouseFPS
) where


import Data.IORef
import Input.Mouse.Types
import Input.Mouse
import qualified Window.Types as W




-- | Mouse reference.
type GLUTMouse = IORef Mouse




-- | Initialize GLUT mouse settings and callbacks.
initilizeCallbacks :: IORef W.Window -> GLUTMouse -> IO ()
initilizeCallbacks wRef mRef = do
    m <- readIORef mRef
    case m of
        ClickOnly {} -> GLUT.mouseCallback $= Just (mouseClick mRef)
        DownOnly {} -> do
            GLUT.mouseCallback  $= Just (mouseClick mRef)
            GLUT.motionCallback $= Just (mouseMotionDown mRef)
        Tracking {} -> do
            GLUT.mouseCallback         $= Just (mouseClick mRef)
            GLUT.motionCallback        $= Just (mouseMotionDown wRef mRef)
            GLUT.passiveMotionCallback $= Just (mouseMotionUp wRef mRef)
        FPS {} -> do
            GLUT.mouseCallback         $= Just (mouseClick mRef)
            GLUT.motionCallback        $= Just (mouseMotionDown wRef mRef)
            GLUT.passiveMotionCallback $= Just (mouseMotionUp wRef mRef)




-- type MouseCallback = MouseButton -> KeyState -> Position -> IO ()

-- | GLUT Mouse click callback function.
mouseClick :: GLUTMouse -> GLUT.MouseCallback

-- Handle mouse wheel up.
mouseClick mRef GLUT.WheelUp GLUT.Down _ = modifyIORef mRef ( m ->
        m { wheelTicks = wheelTicks m + 1 }
    )

-- Handle mouse wheel dow.
mouseClick mRef GLUT.WheelDown GLUT.Down _ = modifyIORef mRef ( m ->
        m { wheelTicks = wheelTicks m - 1 }
    )

-- Handle button down events.
mouseClick mRef button GLUT.Down (GLUT.Position x y) = 
    modifyIORef mRef (m -> case m of
        ClickOnly { buttons = b } ->
            m { buttons = changeButtonState button ButtonPressed b
              , downPosition = Position (fromIntegral x) (fromIntegral y)
              }
        DownOnly { buttons = b } ->
            m { buttons = changeButtonState button ButtonPressed b
              , downPosition = Position (fromIntegral x) (fromIntegral y)
              }
        Tracking { buttons = b } ->
            m { buttons = changeButtonState button ButtonPressed b
              , downPosition = Position (fromIntegral x) (fromIntegral y)
              }
        FPS {} -> m -- Do nothing.
    )

-- Handle button up events.
mouseClick mRef button GLUT.Up (GLUT.Position x y) = 
    modifyIORef mRef (m -> case m of
        ClickOnly { buttons = b } ->
            m { buttons = changeButtonState button ButtonReleased b
              , upPosition = Position (fromIntegral x) (fromIntegral y)
              }
        DownOnly { buttons = b } ->
            m { buttons = changeButtonState button ButtonReleased b
              , upPosition = Position (fromIntegral x) (fromIntegral y)
              }
        Tracking { buttons = b } ->
            m { buttons = changeButtonState button ButtonReleased b
              , upPosition = Position (fromIntegral x) (fromIntegral y)
              }
        FPS {} -> m -- Do nothing.
    )





-- | Change the state of a mouse button.
changeButtonState :: GLUT.MouseButton -> ButtonState -> Buttons -> Buttons
changeButtonState GLUT.LeftButton ns b   = b { leftButton   = ns }
changeButtonState GLUT.MiddleButton ns b = b { middleButton = ns }
changeButtonState GLUT.RightButton ns b  = b { rightButton  = ns }
changeButtonState _ _ b                  = b




-- type MotionCallback = Position -> IO ()

-- | GLUT Mouse motion (button pressed) callback.
mouseMotionDown :: IORef W.Window -> GLUTMouse -> GLUT.MouseMotion
mouseMotionDown wRef mRef (GLUT.Position x y) = do
    mouse <- readIORef mRef
    if mouse == FPS {}
        then readIORef wRef >>= (\x -> resetPointer x)
        else return ()
    modifyIORef mRef (\m ->
        let np = Position x y
        in case m of
            ClickOnly {} -> m -- Do nothing.
            DownOnly { position = op } ->
                m { position = np, deltaPosition = diffMouse op np }
            Tracking { position = op } ->
                m { position = np, deltaPosition = diffMouse op np }
            FPS { position = op } ->
                m { position = np, deltaPosition = diffMouse op np }
    )




-- | GLUT Mouse motion (button released) callback.
mouseMotionUp :: IORef W.Window GLUTMouse -> GLUT.MouseMotion
mouseMotionUp wRef mRef (GLUT.Position x y) =
    mouse <- readIORef mRef
    if mouse == FPS {}
        then readIORef wRef >>= (\x -> resetPointer x)
        else return ()
    modifyIORef mRef (\m ->
        let np = Position x y
        in case m of
            ClickOnly {} -> m -- Do nothing.
            DownOnly {} -> m -- Do nothing.
            Tracking { position = op } ->
                m { position = np, deltaPosition = diffMouse op np }
            FPS { position = op } ->
                m { position = np, deltaPosition = diffMouse op np }
    )




-- | Reset pointer to the center of the screen.
resetPointer :: W.Window -> IO ()
resetPointer w = pointerPosition GLUT.Position (fromIntegral . W.width $ w)/2
                                               (fromIntegral . W.height $ w)/2

