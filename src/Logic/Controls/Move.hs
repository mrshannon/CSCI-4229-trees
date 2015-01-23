{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Logic.Controls.Move
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Controls module (movement).
-------------------------------------------------------------------------------


module Logic.Controls.Move
( handleMove
) where



import Math.Vector.Types
import qualified App.Types as A
import qualified View.Types as V
import qualified Time.Types as T
import qualified Input.Keyboard.Types as K





-- Handle view rotation.
handleMove :: A.App -> A.App
handleMove = handleForwardBackward . handleLeftRight . handleUpDown




-- Movement distance.
delta :: A.App -> Double
delta app = 10 * (T.deltaTime . A.time $ app)




-- Handle w and s keys.
handleForwardBackward :: A.App -> A.App
handleForwardBackward app@(A.App { A.keyboard = (K.Keyboard { K.keys = k }) })
    | K.wKey k  <= K.KeyDown = move (delta app)            0.0 0.0 app
    | K.sKey k  <= K.KeyDown = move (negate . delta $ app) 0.0 0.0 app
    | otherwise              = app




-- Handle a and d keys.
handleLeftRight :: A.App -> A.App
handleLeftRight app@(A.App { A.keyboard = (K.Keyboard { K.keys = k }) })
        | K.aKey k  <= K.KeyDown = move 0.0 (negate . delta $ app) 0.0 app
        | K.dKey k  <= K.KeyDown = move 0.0 (delta app) 0.0 app
        | otherwise              = app




handleUpDown :: A.App -> A.App
handleUpDown app@(A.App { A.keyboard = (K.Keyboard { K.keys = k }) })
    | K.spaceKey k     <= K.KeyDown = move 0.0 0.0 (delta app) app
    | K.leftShiftKey k <= K.KeyDown = move 0.0 0.0 (negate . delta $ app) app
    | otherwise                     = app




-- Handle movement.
move :: Double -> Double -> Double -> A.App -> A.App

-- For first person.
-- TODO: Implement first person camera.
move _ _ _ app@(A.App { A.view = (V.FirstPerson {}) }) = app

-- For orthographic and third person (perspective).
move forwardBackward leftRight upDown app@(A.App { A.view = view }) =
    let azimuth = pi/180 * V.azimuth view
        dx      = negate $ (leftRight * sin azimuth) + (forwardBackward * cos azimuth)
        dy      = (leftRight * cos azimuth) - (forwardBackward * sin azimuth)
        dz      = upDown
        oldSceneCenter = V.sceneCenter view
        newSceneCenter = oldSceneCenter + Vector3 dx dy dz
    in app { A.view = view { V.sceneCenter = newSceneCenter} }

