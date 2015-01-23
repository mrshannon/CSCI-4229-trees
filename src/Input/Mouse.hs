{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Input.Mouse
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Mouse input module.
-------------------------------------------------------------------------------


module Input.Mouse
( cycleMouse
, diffMouse
, defaultMouseClickOnly
, defaultMouseDownOnly
, defaultMouseTracking
, defaultMouseFPS
, Mouse(..)
, Buttons(..)
, ButtonState(..)
, Position(..)
, DeltaPosition(..)
) where


import Input.Mouse.Types




-- Change button state from pressed to down and released to up.
cycleButton :: ButtonState -> ButtonState
cycleButton bs = if bs <= ButtonDown then ButtonDown else ButtonUp




-- Set all ButtonPressed values to ButtonDown and the ButtonReleased values to
-- ButtonUp.  It also resets the wheel ticks to 0.
cycleMouse :: Mouse -> Mouse
cycleMouse m = m { buttons = newButtons, wheelTicks = 0 }
    where
        b = buttons m
        newButtons = b { leftButton   = cycleButton $ leftButton   b
                       , middleButton = cycleButton $ middleButton b
                       , rightButton  = cycleButton $ rightButton  b
                       }




-- | Calculate difference between two mouse positions.
diffMouse :: Position -> Position -> DeltaPosition
diffMouse (Position x0 y0) (Position x1 y1) = 
    DeltaPosition ( fromIntegral x1 - fromIntegral x0 )
                  ( fromIntegral y1 - fromIntegral y0 )




-- Default button state (all buttons up).
defaultButtons :: Buttons
defaultButtons = Buttons
    { leftButton   = ButtonUp
    , middleButton = ButtonUp
    , rightButton  = ButtonUp
    }




-- Default ClickOnly configuration.
defaultMouseClickOnly :: Mouse
defaultMouseClickOnly = ClickOnly
    { buttons      = defaultButtons
    , wheelTicks   = 0
    , downPosition = Position 0 0
    , upPosition   = Position 0 0
    }




-- Default DownOnly configuration.
defaultMouseDownOnly :: Mouse
defaultMouseDownOnly = DownOnly
    { buttons         = defaultButtons
    , wheelTicks      = 0
    , downPosition    = Position 0 0
    , upPosition      = Position 0 0
    , position = Position 0 0
    , deltaPosition   = DeltaPosition 0 0
    }




-- Default Tracking configuration.
defaultMouseTracking :: Mouse
defaultMouseTracking = Tracking
    { buttons         = defaultButtons
    , wheelTicks      = 0
    , downPosition    = Position 0 0
    , upPosition      = Position 0 0
    , position = Position 0 0
    , deltaPosition   = DeltaPosition 0 0
    }




-- Default FPS configuration.
defaultMouseFPS :: Mouse
defaultMouseFPS = FPS
    { buttons       = defaultButtons
    , wheelTicks    = 0
    , deltaPosition = DeltaPosition 0 0
    }

