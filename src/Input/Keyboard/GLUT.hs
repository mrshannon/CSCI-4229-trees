{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Input.Keyboard.GLUT
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Keyboard input module (GLUT adapter).
-------------------------------------------------------------------------------


module Input.Keyboard.GLUT
( Keyboard(..)
, Keys(..)
, KeyState(..)
, initilizeKeyboard
, keyboardDown
, keyboardUp
, specialDown
, specialUp
, cycleKeyboard
) where


import Data.Char
import Data.IORef
import qualified Graphics.UI.GLUT as GLUT
import Graphics.UI.GLUT(($=))
import Input.Keyboard.Types
import Input.Keyboard




-- | Keyboard reference.
type GLUTKeyboard = IORef Keyboard




-- | Initialize GLUT keyboard settings and callbacks.
initilizeKeyboard :: GLUTKeyboard -> IO ()
initilizeKeyboard kbRef = do
    kb <- readIORef kbRef
    GLUT.perWindowKeyRepeat $= if repeatKeys kb
        then GLUT.PerWindowKeyRepeatOn
        else GLUT.PerWindowKeyRepeatOff
    GLUT.keyboardCallback   $= Just (keyboardDown kbRef)
    GLUT.keyboardUpCallback $= Just (keyboardUp kbRef)
    GLUT.specialCallback    $= Just (specialDown kbRef)
    GLUT.specialUpCallback  $= Just (specialUp kbRef)




-- type KeyboardCallback = Char -> Position -> IO ()

-- | GLUT keyboard down callback function.
keyboardDown :: GLUTKeyboard -> GLUT.KeyboardCallback
keyboardDown keyboardRef char _ = modifyIORef keyboardRef (\kb ->
        let newBuffer = if repeatKeys kb 
                then addToBuffer char $ buffer kb
                else buffer kb
            newKeys = changeAsciiKeyState char KeyPressed $ keys kb
        in kb { buffer = newBuffer, keys = newKeys }
    )




-- | GLUT keyboard up callback function.
keyboardUp :: GLUTKeyboard -> GLUT.KeyboardCallback
keyboardUp keyboardRef char _ = modifyIORef keyboardRef (\kb ->
        kb { keys = changeAsciiKeyState char KeyReleased $ keys kb }
    )




-- type SpecialCallback = SpecialKey -> Position -> IO ()

-- | GLUT special key up callback function.
specialDown :: GLUTKeyboard -> GLUT.SpecialCallback
specialDown keyboardRef key _ = modifyIORef keyboardRef (\kb ->
        kb { keys = changeSpecialKeyState key KeyPressed $ keys kb }
    )




-- | GLUT special key up callback function.
specialUp :: GLUTKeyboard -> GLUT.SpecialCallback
specialUp keyboardRef key _ = modifyIORef keyboardRef (\kb ->
        kb { keys = changeSpecialKeyState key KeyReleased $ keys kb }
    )




-- Set ASCII key state.
changeAsciiKeyState :: Char -> KeyState -> Keys -> Keys

-- Leters (upper and lower case).
changeAsciiKeyState 'a' ns k = k { aKey = ns }
changeAsciiKeyState 'A' ns k = k { aKey = ns }
changeAsciiKeyState 'b' ns k = k { bKey = ns }
changeAsciiKeyState 'B' ns k = k { bKey = ns }
changeAsciiKeyState 'c' ns k = k { cKey = ns }
changeAsciiKeyState 'C' ns k = k { cKey = ns }
changeAsciiKeyState 'd' ns k = k { dKey = ns }
changeAsciiKeyState 'D' ns k = k { dKey = ns }
changeAsciiKeyState 'e' ns k = k { eKey = ns }
changeAsciiKeyState 'E' ns k = k { eKey = ns }
changeAsciiKeyState 'f' ns k = k { fKey = ns }
changeAsciiKeyState 'F' ns k = k { fKey = ns }
changeAsciiKeyState 'g' ns k = k { gKey = ns }
changeAsciiKeyState 'G' ns k = k { gKey = ns }
changeAsciiKeyState 'h' ns k = k { hKey = ns }
changeAsciiKeyState 'H' ns k = k { hKey = ns }
changeAsciiKeyState 'i' ns k = k { iKey = ns }
changeAsciiKeyState 'I' ns k = k { iKey = ns }
changeAsciiKeyState 'j' ns k = k { jKey = ns }
changeAsciiKeyState 'J' ns k = k { jKey = ns }
changeAsciiKeyState 'k' ns k = k { kKey = ns }
changeAsciiKeyState 'K' ns k = k { kKey = ns }
changeAsciiKeyState 'l' ns k = k { lKey = ns }
changeAsciiKeyState 'L' ns k = k { lKey = ns }
changeAsciiKeyState 'm' ns k = k { mKey = ns }
changeAsciiKeyState 'M' ns k = k { mKey = ns }
changeAsciiKeyState 'n' ns k = k { nKey = ns }
changeAsciiKeyState 'N' ns k = k { nKey = ns }
changeAsciiKeyState 'o' ns k = k { oKey = ns }
changeAsciiKeyState 'O' ns k = k { oKey = ns }
changeAsciiKeyState 'p' ns k = k { pKey = ns }
changeAsciiKeyState 'P' ns k = k { pKey = ns }
changeAsciiKeyState 'q' ns k = k { qKey = ns }
changeAsciiKeyState 'Q' ns k = k { qKey = ns }
changeAsciiKeyState 'r' ns k = k { rKey = ns }
changeAsciiKeyState 'R' ns k = k { rKey = ns }
changeAsciiKeyState 's' ns k = k { sKey = ns }
changeAsciiKeyState 'S' ns k = k { sKey = ns }
changeAsciiKeyState 't' ns k = k { tKey = ns }
changeAsciiKeyState 'T' ns k = k { tKey = ns }
changeAsciiKeyState 'u' ns k = k { uKey = ns }
changeAsciiKeyState 'U' ns k = k { uKey = ns }
changeAsciiKeyState 'v' ns k = k { vKey = ns }
changeAsciiKeyState 'V' ns k = k { vKey = ns }
changeAsciiKeyState 'w' ns k = k { wKey = ns }
changeAsciiKeyState 'W' ns k = k { wKey = ns }
changeAsciiKeyState 'x' ns k = k { xKey = ns }
changeAsciiKeyState 'X' ns k = k { xKey = ns }
changeAsciiKeyState 'y' ns k = k { yKey = ns }
changeAsciiKeyState 'Y' ns k = k { yKey = ns }
changeAsciiKeyState 'z' ns k = k { zKey = ns }
changeAsciiKeyState 'Z' ns k = k { zKey = ns }

-- Numbers and symbols.
changeAsciiKeyState '0' ns k = k { key0 = ns }
changeAsciiKeyState ')' ns k = k { key0 = ns }
changeAsciiKeyState '1' ns k = k { key1 = ns }
changeAsciiKeyState '!' ns k = k { key1 = ns }
changeAsciiKeyState '2' ns k = k { key2 = ns }
changeAsciiKeyState '@' ns k = k { key2 = ns }
changeAsciiKeyState '3' ns k = k { key3 = ns }
changeAsciiKeyState '#' ns k = k { key3 = ns }
changeAsciiKeyState '4' ns k = k { key4 = ns }
changeAsciiKeyState '$' ns k = k { key4 = ns }
changeAsciiKeyState '5' ns k = k { key5 = ns }
changeAsciiKeyState '%' ns k = k { key5 = ns }
changeAsciiKeyState '6' ns k = k { key6 = ns }
changeAsciiKeyState '^' ns k = k { key6 = ns }
changeAsciiKeyState '7' ns k = k { key7 = ns }
changeAsciiKeyState '&' ns k = k { key7 = ns }
changeAsciiKeyState '8' ns k = k { key8 = ns }
changeAsciiKeyState '*' ns k = k { key8 = ns }
changeAsciiKeyState '9' ns k = k { key9 = ns }
changeAsciiKeyState '(' ns k = k { key9 = ns }

-- Symbols.
changeAsciiKeyState '`'  ns k = k { backtickKey     = ns }
changeAsciiKeyState '~'  ns k = k { backtickKey     = ns }
changeAsciiKeyState '-'  ns k = k { hyphenKey       = ns }
changeAsciiKeyState '_'  ns k = k { hyphenKey       = ns }
changeAsciiKeyState '='  ns k = k { equalsKey       = ns }
changeAsciiKeyState '+'  ns k = k { equalsKey       = ns }
changeAsciiKeyState '['  ns k = k { leftBracketKey  = ns }
changeAsciiKeyState '{'  ns k = k { leftBracketKey  = ns }
changeAsciiKeyState ']'  ns k = k { rightBracketKey = ns }
changeAsciiKeyState '}'  ns k = k { rightBracketKey = ns }
changeAsciiKeyState '\\' ns k = k { backslashKey    = ns }
changeAsciiKeyState '|'  ns k = k { backslashKey    = ns }
changeAsciiKeyState ';'  ns k = k { semicolonKey    = ns }
changeAsciiKeyState ':'  ns k = k { semicolonKey    = ns }
changeAsciiKeyState '\'' ns k = k { apostropheKey   = ns }
changeAsciiKeyState '"'  ns k = k { apostropheKey   = ns }
changeAsciiKeyState ','  ns k = k { commaKey        = ns }
changeAsciiKeyState '<'  ns k = k { commaKey        = ns }
changeAsciiKeyState '.'  ns k = k { periodKey       = ns }
changeAsciiKeyState '>'  ns k = k { periodKey       = ns }
changeAsciiKeyState '/'  ns k = k { slashKey        = ns }
changeAsciiKeyState '?'  ns k = k { slashKey        = ns }
changeAsciiKeyState ' '  ns k = k { spaceKey        = ns }
changeAsciiKeyState '\t' ns k = k { tabKey          = ns }
changeAsciiKeyState '\r' ns k = k { enterKey        = ns }

-- Non type-able characters.
changeAsciiKeyState c ns k = case ord c of
    127 -> k { deleteKey    = ns } -- DEL (delete)
    27  -> k { escapeKey    = ns } -- ESC (escape)
    8   -> k { backspaceKey = ns } -- BS  (backspace)
    _   -> k                       -- no match




-- | Set special key state.
changeSpecialKeyState :: GLUT.SpecialKey -> KeyState -> Keys -> Keys

-- Function keys.
changeSpecialKeyState GLUT.KeyF1  ns k = k { f1Key         = ns }
changeSpecialKeyState GLUT.KeyF2  ns k = k { f2Key         = ns }
changeSpecialKeyState GLUT.KeyF3  ns k = k { f3Key         = ns }
changeSpecialKeyState GLUT.KeyF4  ns k = k { f4Key         = ns }
changeSpecialKeyState GLUT.KeyF5  ns k = k { f5Key         = ns }
changeSpecialKeyState GLUT.KeyF6  ns k = k { f6Key         = ns }
changeSpecialKeyState GLUT.KeyF7  ns k = k { f7Key         = ns }
changeSpecialKeyState GLUT.KeyF8  ns k = k { f8Key         = ns }
changeSpecialKeyState GLUT.KeyF9  ns k = k { f9Key         = ns }
changeSpecialKeyState GLUT.KeyF10 ns k = k { f10Key        = ns }
changeSpecialKeyState GLUT.KeyF11 ns k = k { f11Key        = ns }
changeSpecialKeyState GLUT.KeyF12 ns k = k { f12Key        = ns }

-- Movement keys.
changeSpecialKeyState GLUT.KeyLeft     ns k = k { leftKey       = ns }
changeSpecialKeyState GLUT.KeyRight    ns k = k { rightKey      = ns }
changeSpecialKeyState GLUT.KeyUp       ns k = k { upKey         = ns }
changeSpecialKeyState GLUT.KeyDown     ns k = k { downKey       = ns }
changeSpecialKeyState GLUT.KeyPageUp   ns k = k { pageUpKey     = ns }
changeSpecialKeyState GLUT.KeyPageDown ns k = k { pageDownKey   = ns }

-- Modifier keys.
changeSpecialKeyState GLUT.KeyShiftL ns k = k { leftShiftKey  = ns }
changeSpecialKeyState GLUT.KeyShiftR ns k = k { rightShiftKey = ns }
changeSpecialKeyState GLUT.KeyCtrlL  ns k = k { leftCtrlKey   = ns }
changeSpecialKeyState GLUT.KeyCtrlR  ns k = k { rightCtrlKey  = ns }
changeSpecialKeyState GLUT.KeyAltL   ns k = k { leftAltKey    = ns }
changeSpecialKeyState GLUT.KeyAltR   ns k = k { rightAltKey   = ns }

-- Special keys.
changeSpecialKeyState GLUT.KeyHome    ns k = k { homeKey   = ns }
changeSpecialKeyState GLUT.KeyEnd     ns k = k { endKey    = ns }
changeSpecialKeyState GLUT.KeyInsert  ns k = k { insertKey = ns }

-- No match.
changeSpecialKeyState _ _ k = k

