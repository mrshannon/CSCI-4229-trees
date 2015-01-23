{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Input.Keyboard
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Keyboard input module.
-------------------------------------------------------------------------------


module Input.Keyboard
( KeyState(..)
, Keyboard(..)
, Keys(..)
, cycleKeyboard
, addToBuffer
, defaultKeyboard
) where


import Input.Keyboard.Types




-- | Add character to the buffer.
addToBuffer :: Char -> String -> String
addToBuffer c b = c:b




-- | Default keyboard state.
defaultKeyboard :: Keyboard
defaultKeyboard   = Keyboard
    { repeatKeys  = False
    , bufferChars = True
    , buffer      = []
    , keys        = defaultKeys
    }




-- | Change key state from pressed to down and released to up.
cycleKey :: KeyState -> KeyState
cycleKey ks = if ks <= KeyDown then KeyDown else KeyUp




-- | Set all KeyPressed values to KeyDown and the KeyReleased values to KeyUp.
cycleKeyboard :: Keyboard -> Keyboard
cycleKeyboard kb = kb { keys = newKeys }
    where
        k       = keys kb
        newKeys = k { 
                    -- Letters.
                      aKey = cycleKey $ aKey k
                    , bKey = cycleKey $ bKey k
                    , cKey = cycleKey $ cKey k
                    , dKey = cycleKey $ dKey k
                    , eKey = cycleKey $ eKey k
                    , fKey = cycleKey $ fKey k
                    , gKey = cycleKey $ gKey k
                    , hKey = cycleKey $ hKey k
                    , iKey = cycleKey $ iKey k
                    , jKey = cycleKey $ jKey k
                    , kKey = cycleKey $ kKey k
                    , lKey = cycleKey $ lKey k
                    , mKey = cycleKey $ mKey k
                    , nKey = cycleKey $ nKey k
                    , oKey = cycleKey $ oKey k
                    , pKey = cycleKey $ pKey k
                    , qKey = cycleKey $ qKey k
                    , rKey = cycleKey $ rKey k
                    , sKey = cycleKey $ sKey k
                    , tKey = cycleKey $ tKey k
                    , uKey = cycleKey $ uKey k
                    , vKey = cycleKey $ vKey k
                    , wKey = cycleKey $ wKey k
                    , xKey = cycleKey $ xKey k
                    , yKey = cycleKey $ yKey k
                    , zKey = cycleKey $ zKey k

                    -- Numbers and symbols.
                    , key0 = cycleKey $ key0 k
                    , key1 = cycleKey $ key1 k
                    , key2 = cycleKey $ key2 k
                    , key3 = cycleKey $ key3 k
                    , key4 = cycleKey $ key4 k
                    , key5 = cycleKey $ key5 k
                    , key6 = cycleKey $ key6 k
                    , key7 = cycleKey $ key7 k
                    , key8 = cycleKey $ key8 k
                    , key9 = cycleKey $ key9 k

                    -- Symbols.
                    , backtickKey     = cycleKey $ backtickKey k
                    , hyphenKey       = cycleKey $ hyphenKey k
                    , equalsKey       = cycleKey $ equalsKey k
                    , leftBracketKey  = cycleKey $ leftBracketKey k
                    , rightBracketKey = cycleKey $ rightBracketKey k
                    , backslashKey    = cycleKey $ backslashKey k
                    , semicolonKey    = cycleKey $ semicolonKey k
                    , apostropheKey   = cycleKey $ apostropheKey k
                    , commaKey        = cycleKey $ commaKey k
                    , periodKey       = cycleKey $ periodKey k
                    , slashKey        = cycleKey $ slashKey k
                    , spaceKey        = cycleKey $ spaceKey k
                    , enterKey        = cycleKey $ enterKey k
                    , deleteKey       = cycleKey $ deleteKey k
                    , escapeKey       = cycleKey $ escapeKey k
                    , backspaceKey    = cycleKey $ backspaceKey k
                    }




-- | Default key state (all keys up).
defaultKeys :: Keys
defaultKeys = Keys
    {
    -- Regular ASCII Keys.
      aKey            = KeyUp -- a/A
    , bKey            = KeyUp -- b/B
    , cKey            = KeyUp -- c/C
    , dKey            = KeyUp -- d/D
    , eKey            = KeyUp -- e/E
    , fKey            = KeyUp -- f/F
    , gKey            = KeyUp -- g/G
    , hKey            = KeyUp -- h/H
    , iKey            = KeyUp -- i/I
    , jKey            = KeyUp -- j/J
    , kKey            = KeyUp -- k/K
    , lKey            = KeyUp -- l/L
    , mKey            = KeyUp -- m/M
    , nKey            = KeyUp -- n/N
    , oKey            = KeyUp -- o/O
    , pKey            = KeyUp -- p/P
    , qKey            = KeyUp -- q/Q
    , rKey            = KeyUp -- r/R
    , sKey            = KeyUp -- s/S
    , tKey            = KeyUp -- t/T
    , uKey            = KeyUp -- u/U
    , vKey            = KeyUp -- v/V
    , wKey            = KeyUp -- w/W
    , xKey            = KeyUp -- x/X
    , yKey            = KeyUp -- y/Y
    , zKey            = KeyUp -- z/Z
    , key0            = KeyUp -- 0/)
    , key1            = KeyUp -- 1/!
    , key2            = KeyUp -- 2/@
    , key3            = KeyUp -- 3/#
    , key4            = KeyUp -- 4/$
    , key5            = KeyUp -- 5/%
    , key6            = KeyUp -- 6/^
    , key7            = KeyUp -- 7/&
    , key8            = KeyUp -- 8/*
    , key9            = KeyUp -- 9/(
    , backtickKey     = KeyUp -- `/~
    , hyphenKey       = KeyUp -- -/_
    , equalsKey       = KeyUp -- =/+
    , leftBracketKey  = KeyUp -- [/{
    , rightBracketKey = KeyUp -- ]/}
    , backslashKey    = KeyUp -- \/|
    , semicolonKey    = KeyUp -- ;/:
    , apostropheKey   = KeyUp -- '/"
    , commaKey        = KeyUp -- ,/<
    , periodKey       = KeyUp -- ./>
    , slashKey        = KeyUp -- //?
    , tabKey          = KeyUp -- TAB
    , spaceKey        = KeyUp -- SPACE
    , escapeKey       = KeyUp -- ESC
    , enterKey        = KeyUp -- CR
    , deleteKey       = KeyUp -- DEL
    , backspaceKey    = KeyUp -- BS

    -- Function keys.
    , f1Key  = KeyUp -- F1
    , f2Key  = KeyUp -- F2
    , f3Key  = KeyUp -- F3
    , f4Key  = KeyUp -- F4
    , f5Key  = KeyUp -- F5
    , f6Key  = KeyUp -- F6
    , f7Key  = KeyUp -- F7
    , f8Key  = KeyUp -- F8
    , f9Key  = KeyUp -- F9
    , f10Key = KeyUp -- F10
    , f11Key = KeyUp -- F11
    , f12Key = KeyUp -- F12

    -- Arrow and page keys.
    , leftKey     = KeyUp -- left arrow
    , rightKey    = KeyUp -- right arrow
    , upKey       = KeyUp -- up arrow
    , downKey     = KeyUp -- down arrow
    , pageUpKey   = KeyUp -- page up
    , pageDownKey = KeyUp -- page down

    -- Modifer keys.
    , leftShiftKey  = KeyUp -- left shift
    , rightShiftKey = KeyUp -- right shift
    , leftCtrlKey   = KeyUp -- left ctrl
    , rightCtrlKey  = KeyUp -- right ctrl
    , leftAltKey    = KeyUp -- left alt
    , rightAltKey   = KeyUp -- right alt

    -- Special keys.
    , homeKey   = KeyUp -- home
    , endKey    = KeyUp -- end
    , insertKey = KeyUp -- insert

    }

