{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : App.Types
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- App module types.
-------------------------------------------------------------------------------


module App.Types
( App(..)
, RefApp(..)
) where


import Data.IORef
import Input.Keyboard.Types(Keyboard)
import Input.Mouse.Types(Mouse)
import Window.Types(Window)
import View.Types(View)
import Settings.Types(Settings)
import World.Types(World)
import Time.Types(Time)




data App = App -- Contains all the pieces of the application.
        { keyboard  :: Keyboard
        , mouse     :: Mouse
        , window    :: Window
        , view      :: View
        , time      :: Time
        , settings  :: Settings
        , world     :: World
        , initFun   :: App -> IO App
        , updateFun :: App -> App
        , drawFun   :: App -> IO ()
        }




data RefApp = RefApp -- Contains IORef's to each piece of the application.
        { keyboardRef  :: IORef Keyboard
        , mouseRef     :: IORef Mouse
        , windowRef    :: IORef Window
        , viewRef      :: IORef View
        , timeRef      :: IORef Time
        , settingsRef  :: IORef Settings
        , worldRef     :: IORef World
        , initFunRef   :: IORef (App -> IO App)
        , updateFunRef :: IORef (App -> App)
        , drawFunRef   :: IORef (App -> IO ())
        }

