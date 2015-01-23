{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : World.Types
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- World module types.
-------------------------------------------------------------------------------


module World.Types
( World(..)
, AnimationState(..)
, Sun(..)
) where


import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Skybox as Sky



data World = World
    { animationState  :: AnimationState
    , sun             :: Sun
    , sunTexture      :: Maybe GL.TextureObject
    , groundTexture   :: Maybe GL.TextureObject
    , treeBarkTexture :: Maybe GL.TextureObject
    , leaves1Texture  :: Maybe GL.TextureObject
    , leaves2Texture  :: Maybe GL.TextureObject
    , skyboxTextures  :: Maybe Sky.Skybox
    } deriving(Eq, Show)




data AnimationState = Running | Paused deriving(Eq, Show)




data Sun = Sun
    { inclination :: Double
    , elevation   :: Double
    } deriving(Eq, Show)

