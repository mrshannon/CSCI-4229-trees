{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Logic.Controls.Rotation
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Controls module (view rotation).
-------------------------------------------------------------------------------


module Logic.Controls.Rotate
( rotate
) where


import qualified App.Types as A
import qualified Input.Keyboard.Types as K
import qualified View.Types as V
import qualified Time.Types as T




-- Handle view rotation.
rotate :: A.App -> A.App
rotate = rotateUpDown . rotateLeftRight




-- Rotation speed scale factor.
rotationScale :: Double
rotationScale = 25.0




-- Handle up/down view rotation.
rotateUpDown :: A.App -> A.App
rotateUpDown app@(A.App { A.keyboard = (K.Keyboard { K.keys = k }) })
    | K.upKey k    <= K.KeyDown = rotateUp app
    | K.downKey k  <= K.KeyDown = rotateDown app
    | otherwise                 = app




-- Handle left rotation.
rotateLeft :: A.App -> A.App

-- For orthographic view.
rotateLeft app@(A.App { A.view = view@(V.Orthographic {}) }) =
    let oldAzimuth = V.azimuth view
        newAzimuth = oldAzimuth + rotationScale*(T.deltaTime . A.time $ app)
    in app { A.view = view { V.azimuth = newAzimuth } }

-- For third person (perspective) view.
rotateLeft app@(A.App { A.view = view@(V.ThirdPerson {}) }) =
    let oldAzimuth = V.azimuth view
        newAzimuth = oldAzimuth + rotationScale*(T.deltaTime . A.time $ app)
    in app { A.view = view { V.azimuth = newAzimuth } }

-- For first person view.
-- TODO: First person view not implemented yet.
rotateLeft app@(A.App { A.view = (V.FirstPerson {}) }) = app




-- Handle right rotation.
rotateRight :: A.App -> A.App

-- For orthographic view.
rotateRight app@(A.App { A.view = view@(V.Orthographic {}) }) =
    let oldAzimuth = V.azimuth view
        newAzimuth = oldAzimuth - rotationScale*(T.deltaTime . A.time $ app)
    in app { A.view = view { V.azimuth = newAzimuth } }

-- For third person (perspective) view.
rotateRight app@(A.App { A.view = view@(V.ThirdPerson {}) }) =
    let oldAzimuth = V.azimuth view
        newAzimuth = oldAzimuth - rotationScale*(T.deltaTime . A.time $ app)
    in app { A.view = view { V.azimuth = newAzimuth } }

-- For first person view.
-- TODO: First person view not implemented yet.
rotateRight app@(A.App { A.view = (V.FirstPerson {}) }) = app




-- Handle left/right view rotation.
rotateLeftRight :: A.App -> A.App
rotateLeftRight app@(A.App { A.keyboard = (K.Keyboard { K.keys = k }) })
    | K.leftKey k  <= K.KeyDown = rotateLeft app
    | K.rightKey k <= K.KeyDown = rotateRight app
    | otherwise                 = app




-- Handle up rotation.
rotateUp :: A.App -> A.App

-- For orthographic view.
rotateUp app@(A.App { A.view = view@(V.Orthographic {}) }) =
    let oldElevation = V.elevation view
        newElevation = oldElevation - rotationScale*(T.deltaTime . A.time $ app)
    in app { A.view = view { V.elevation = newElevation } }

-- For third person (perspective) view.
rotateUp app@(A.App { A.view = view@(V.ThirdPerson {}) }) =
    let oldElevation = V.elevation view
        newElevation = oldElevation - rotationScale*(T.deltaTime . A.time $ app)
    in app { A.view = view { V.elevation = newElevation } }

-- For first person view.
-- TODO: First person view not implemented yet.
rotateUp app@(A.App { A.view = (V.FirstPerson {}) }) = app




-- Handle down rotation.
rotateDown :: A.App -> A.App

-- For orthographic view.
rotateDown app@(A.App { A.view = view@(V.Orthographic {}) }) =
    let oldElevation = V.elevation view
        newElevation = oldElevation + rotationScale*(T.deltaTime . A.time $ app)
    in app { A.view = view { V.elevation = newElevation } }

-- For third person (perspective) view.
rotateDown app@(A.App { A.view = view@(V.ThirdPerson {}) }) =
    let oldElevation = V.elevation view
        newElevation = oldElevation + rotationScale*(T.deltaTime . A.time $ app)
    in app { A.view = view { V.elevation = newElevation } }

-- For first person view.
-- TODO: First person view not implemented yet.
rotateDown app@(A.App { A.view = (V.FirstPerson {}) }) = app

