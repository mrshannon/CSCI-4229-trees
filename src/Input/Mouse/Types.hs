{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Input.Mouse.Types
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Mouse input module types.
-------------------------------------------------------------------------------


module Input.Mouse.Types
( Mouse(..)
, Buttons(..)
, ButtonState(..)
, Position(..)
, DeltaPosition(..)
) where


import Data.Word




data Position = Position Word Word deriving(Eq, Show)




data DeltaPosition = DeltaPosition Int Int deriving(Eq, Show)





-- | Button state.
data ButtonState
    = ButtonPressed  -- Button was pressed since last event polling.
    | ButtonDown     -- Button is down.
    | ButtonUp       -- Button was released since last event polling.
    | ButtonReleased -- Button is up.
    deriving(Eq, Ord, Show)




-- State of buttons.
data Buttons = Buttons
    { leftButton   :: ButtonState
    , middleButton :: ButtonState
    , rightButton  :: ButtonState
    } deriving(Eq, Show)




-- Mouse state.
data Mouse
    = ClickOnly -- Only track clicks (press and release).
        { buttons      :: Buttons
        , wheelTicks   :: Int
        , downPosition :: Position
        , upPosition   :: Position
        }
    | DownOnly -- Track current position but only when the mouse is down.
        { buttons         :: Buttons
        , wheelTicks      :: Int
        , downPosition    :: Position
        , upPosition      :: Position
        , position        :: Position
        , deltaPosition   :: DeltaPosition
        }
    | Tracking -- Track current position all the time.
        { buttons         :: Buttons
        , wheelTicks      :: Int
        , downPosition    :: Position
        , upPosition      :: Position
        , position        :: Position
        , deltaPosition   :: DeltaPosition
        }
    | FPS -- FPS camera, only change in position is tracked and pointer is
          -- invisible.
        { buttons       :: Buttons
        , wheelTicks    :: Int
        , deltaPosition :: DeltaPosition
        } deriving(Eq, Show)

