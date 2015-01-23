{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Input.Keyboard.Types
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Keyboard input module types.
-------------------------------------------------------------------------------


module Input.Keyboard.Types
( Keyboard(..)
, Keys(..)
, KeyState(..)
) where




-- | Represents the keyboard.
data Keyboard = Keyboard
    { repeatKeys  :: Bool
    , bufferChars :: Bool
    , buffer      :: String
    , keys        :: Keys
    } deriving(Eq, Show)




-- | Key state.
data KeyState
    = KeyPressed  -- Key was pressed since last event polling.
    | KeyDown     -- Key is down.
    | KeyUp       -- Key was released since last event polling.
    | KeyReleased -- Key is up.
    deriving(Eq, Ord, Show)




-- State of all keyboard keys.
data Keys = Keys
    {
    -- Regular ASCII Keys.
      aKey            :: KeyState -- a/A
    , bKey            :: KeyState -- b/B
    , cKey            :: KeyState -- c/C
    , dKey            :: KeyState -- d/D
    , eKey            :: KeyState -- e/E
    , fKey            :: KeyState -- f/F
    , gKey            :: KeyState -- g/G
    , hKey            :: KeyState -- h/H
    , iKey            :: KeyState -- i/I
    , jKey            :: KeyState -- j/J
    , kKey            :: KeyState -- k/K
    , lKey            :: KeyState -- l/L
    , mKey            :: KeyState -- m/M
    , nKey            :: KeyState -- n/N
    , oKey            :: KeyState -- o/O
    , pKey            :: KeyState -- p/P
    , qKey            :: KeyState -- q/Q
    , rKey            :: KeyState -- r/R
    , sKey            :: KeyState -- s/S
    , tKey            :: KeyState -- t/T
    , uKey            :: KeyState -- u/U
    , vKey            :: KeyState -- v/V
    , wKey            :: KeyState -- w/W
    , xKey            :: KeyState -- x/X
    , yKey            :: KeyState -- y/Y
    , zKey            :: KeyState -- z/Z
    , key0            :: KeyState -- 0/)
    , key1            :: KeyState -- 1/!
    , key2            :: KeyState -- 2/@
    , key3            :: KeyState -- 3/#
    , key4            :: KeyState -- 4/$
    , key5            :: KeyState -- 5/%
    , key6            :: KeyState -- 6/^
    , key7            :: KeyState -- 7/&
    , key8            :: KeyState -- 8/*
    , key9            :: KeyState -- 9/(
    , backtickKey     :: KeyState -- `/~
    , hyphenKey       :: KeyState -- -/_
    , equalsKey       :: KeyState -- =/+
    , leftBracketKey  :: KeyState -- [/{
    , rightBracketKey :: KeyState -- ]/}
    , backslashKey    :: KeyState -- \/|
    , semicolonKey    :: KeyState -- ;/:
    , apostropheKey   :: KeyState -- '/"
    , commaKey        :: KeyState -- ,/<
    , periodKey       :: KeyState -- ./>
    , slashKey        :: KeyState -- //?
    , tabKey          :: KeyState -- TAB
    , spaceKey        :: KeyState -- SPACE
    , escapeKey       :: KeyState -- ESC
    , enterKey        :: KeyState -- CR
    , deleteKey       :: KeyState -- DEL
    , backspaceKey    :: KeyState -- BS

    -- Function keys.
    , f1Key  :: KeyState -- F1
    , f2Key  :: KeyState -- F2
    , f3Key  :: KeyState -- F3
    , f4Key  :: KeyState -- F4
    , f5Key  :: KeyState -- F5
    , f6Key  :: KeyState -- F6
    , f7Key  :: KeyState -- F7
    , f8Key  :: KeyState -- F8
    , f9Key  :: KeyState -- F9
    , f10Key :: KeyState -- F10
    , f11Key :: KeyState -- F11
    , f12Key :: KeyState -- F12

    -- Movement keys.
    , leftKey     :: KeyState -- left arrow
    , rightKey    :: KeyState -- right arrow
    , upKey       :: KeyState -- up arrow
    , downKey     :: KeyState -- down arrow
    , pageUpKey   :: KeyState -- page up
    , pageDownKey :: KeyState -- page down

    -- Modifer keys.
    , leftShiftKey  :: KeyState -- left shift
    , rightShiftKey :: KeyState -- right shift
    , leftCtrlKey   :: KeyState -- left ctrl
    , rightCtrlKey  :: KeyState -- right ctrl
    , leftAltKey    :: KeyState -- left alt
    , rightAltKey   :: KeyState -- right alt

    -- Special keys.
    , homeKey   :: KeyState -- home
    , endKey    :: KeyState -- end
    , insertKey :: KeyState -- insert

    } deriving(Eq, Show)

