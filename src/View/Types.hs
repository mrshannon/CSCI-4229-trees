{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : View.Types
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- View module types.
-------------------------------------------------------------------------------


module View.Types
( View(..)
) where


import Math.Vector.Types




data View
    = Orthographic
        { sceneCenter :: Vector3 Double -- Center of the scene is world coordinates.
        , sceneSize   :: Double         -- 2*cubic size of scene around the center.
        , zFactor     :: Double         -- Scale factor in the z direction.
        , azimuth     :: Double         -- Azimuth from positive x-axis.
        , elevation   :: Double         -- Elevation from xy-plane.
        , tilt        :: Double         -- Tilt angle.
        }
    | ThirdPerson -- Perspective version of Orthographic.
        { sceneCenter   :: Vector3 Double -- Center of the scene is world coordinates.
        , hoverDistance :: Double         -- Eye distance from the center.
        , fov           :: Double         -- Field of view angle.
        , nearPlane     :: Double         -- Near clipping plane.
        , farPlane      :: Double         -- Far clipping plane.
        , azimuth       :: Double         -- Azimuth from positive x-axis.
        , elevation     :: Double         -- Elevation from xy-plane.
        , tilt          :: Double         -- Tilt angle.
        }
    | FirstPerson
        { eyePosition   :: Vector3 Double -- Position of eye in world coordinates.
        , viewDirection :: Vector3 Double -- Direction of camera.
        , upVector      :: Vector3 Double -- Up vector.
        , nearPlane     :: Double         -- Near clipping plane.
        , farPlane      :: Double         -- Far clipping plane.
        , fov           :: Double         -- Field of view angle.
        } deriving(Eq, Show)

