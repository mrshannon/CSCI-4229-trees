{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : App
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Main application module.
-------------------------------------------------------------------------------


module App
( App(..)
, RefApp(..)
, pullRef
, putRef
, makeRef
) where


import Data.IORef
import App.Types




-- Pull the data out of the references and place them in an App structure.
pullRef :: RefApp -> IO App
pullRef refApp = do
    kb  <- readIORef . keyboardRef  $ refApp
    m   <- readIORef . mouseRef     $ refApp
    win <- readIORef . windowRef    $ refApp
    v   <- readIORef . viewRef      $ refApp
    t   <- readIORef . timeRef      $ refApp
    s   <- readIORef . settingsRef  $ refApp
    w   <- readIORef . worldRef     $ refApp
    inf <- readIORef . initFunRef   $ refApp
    uf  <- readIORef . updateFunRef $ refApp
    df  <- readIORef . drawFunRef   $ refApp
    return App 
        { keyboard  = kb
        , mouse     = m
        , window    = win
        , view      = v
        , time      = t
        , settings  = s
        , world     = w
        , initFun   = inf
        , updateFun = uf
        , drawFun   = df
        }




-- Put App data into references.
putRef :: App -> RefApp -> IO ()
putRef app refApp = do
    writeIORef ( keyboardRef refApp  ) ( keyboard app  )
    writeIORef ( mouseRef    refApp  ) ( mouse    app  )
    writeIORef ( windowRef   refApp  ) ( window   app  )
    writeIORef ( viewRef     refApp  ) ( view     app  )
    writeIORef ( timeRef     refApp  ) ( time     app  )
    writeIORef ( settingsRef refApp  ) ( settings app  )
    writeIORef ( worldRef    refApp  ) ( world    app  )
    writeIORef ( initFunRef refApp   ) ( initFun app   )
    writeIORef ( updateFunRef refApp ) ( updateFun app )
    writeIORef ( drawFunRef refApp   ) ( drawFun app   )




makeRef :: App -> IO RefApp
makeRef app = do
    kbRef  <- newIORef $ keyboard  app
    mRef   <- newIORef $ mouse     app
    winRef <- newIORef $ window    app
    vRef   <- newIORef $ view      app
    tRef   <- newIORef $ time      app
    sRef   <- newIORef $ settings  app
    wRef   <- newIORef $ world     app
    infRef <- newIORef $ initFun   app
    ufRef  <- newIORef $ updateFun app
    dfRef  <- newIORef $ drawFun   app
    return RefApp
        { keyboardRef  = kbRef
        , mouseRef     = mRef
        , windowRef    = winRef
        , viewRef      = vRef
        , timeRef      = tRef
        , settingsRef  = sRef
        , worldRef     = wRef
        , initFunRef   = infRef
        , updateFunRef = ufRef
        , drawFunRef   = dfRef
        }



