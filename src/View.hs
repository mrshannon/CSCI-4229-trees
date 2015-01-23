{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : View
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- View module.
-------------------------------------------------------------------------------


module View
( View(..)
, zoom
, toOrthographic
, toThirdPerson
, toFirstPerson
) where


import View.Types
import Math.Vector.Types




-- Zoom by a scale factor while keeping all values in sync.
zoom :: Double -> View -> View

-- For orthographic view.
zoom factor oldView@(Orthographic { sceneSize = oldSceneSize}) =
    oldView { sceneSize = oldSceneSize / factor }

-- For third person (perspective) view.
zoom factor oldView@(ThirdPerson { hoverDistance = oldHoverDistance }) = 
    let newHoverDistance = oldHoverDistance / factor
    in oldView { hoverDistance = newHoverDistance
               }

-- For first person view (no zoom).
zoom _ oldView@(FirstPerson {}) = oldView




-- Converts view to orthographic using sane defaults.
toOrthographic :: View -> View

-- From orthographic.
toOrthographic oldView@(Orthographic {}) = oldView

-- From third person.
toOrthographic oldView@(ThirdPerson {}) =
    Orthographic
        { sceneCenter = sceneCenter oldView
        , sceneSize   = hoverDistance oldView / 2.0
        , zFactor     = 10.0
        , azimuth     = azimuth oldView
        , elevation   = elevation oldView
        , tilt        = tilt oldView
        }

-- From first person.
toOrthographic oldView@(FirstPerson {}) =
    Orthographic
        { sceneCenter = eyePosition oldView
        , sceneSize   = farPlane oldView / 4.0
        , zFactor     = 10.0
        , azimuth     =  0.0
        , elevation   = 45.0
        , tilt        =  0.0
        }




-- Converts view to third person using sane defaults.
toThirdPerson :: View -> View

-- From orthographic.
toThirdPerson oldView@(Orthographic {}) =
    ThirdPerson
        { sceneCenter   = sceneCenter oldView
        , hoverDistance = 2.0 * sceneSize oldView
        , fov           = 55.0
        , nearPlane     = 0.1
        , farPlane      = 1000.0
        , azimuth       = azimuth oldView
        , elevation     = elevation oldView
        , tilt          = tilt oldView
        }

-- From third person.
toThirdPerson oldView@(ThirdPerson {}) = oldView

-- From first person.
toThirdPerson oldView@(FirstPerson {}) =
    ThirdPerson
        { sceneCenter   = eyePosition oldView
        , hoverDistance = farPlane oldView / 4.0
        , fov           = fov oldView
        , nearPlane     = nearPlane oldView
        , farPlane      = farPlane oldView
        , azimuth       = 0.0
        , elevation     = 45.0
        , tilt          = 0.0
        }




-- Converts view to first person using sane defaults.
toFirstPerson :: View -> View

-- From orthographic.
toFirstPerson oldView@(Orthographic {}) =
    FirstPerson
        { eyePosition   = sceneCenter oldView
        , viewDirection = Vector3 1.0 0.0 0.0
        , upVector      = Vector3 0.0 0.0 1.0
        , nearPlane     = sceneSize oldView / 4.0
        , farPlane      = 4.0 * sceneSize oldView
        , fov           = 55.0
        }

-- From third person.
toFirstPerson oldView@(ThirdPerson {}) =
    FirstPerson
        { eyePosition   = sceneCenter oldView
        , viewDirection = Vector3 1.0 0.0 0.0
        , upVector      = Vector3 0.0 0.0 1.0
        , nearPlane     = nearPlane oldView
        , farPlane      = farPlane oldView
        , fov           = fov oldView
        }

-- From first person.
toFirstPerson oldView@(FirstPerson {}) = oldView

