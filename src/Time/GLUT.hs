{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Time.GLUT
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Time module (GLUT adapter).
-------------------------------------------------------------------------------


module Time.GLUT
( updateTime
) where


import qualified Graphics.UI.GLUT as GLUT
import qualified Time as T
import Time.Types


updateTime :: Time -> IO Time
updateTime oldTime = do
    time <- GLUT.get GLUT.elapsedTime
    return . T.updateTime oldTime $ fromIntegral time / 1000.0

