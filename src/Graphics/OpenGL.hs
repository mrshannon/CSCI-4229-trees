{-# OPTIONS_GHC -Wall -fwarn-tabs -Werror #-}

-------------------------------------------------------------------------------
-- |
-- Module       : Graphics.OpenGL
-- Copyright    : Copyright (c) 2014 Michael R. Shannon
-- License      : GPLv2 or Later
-- Maintainer   : mrshannon.aerospace@gmail.com
-- Stability    : unstable
-- Portability  : portable
--
-- Graphics module (OpenGL).
-------------------------------------------------------------------------------


module Graphics.OpenGL
( applyProjection
, applyView
) where


import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL(($=))
import qualified View.Types as V
import qualified Window.Types as W
import qualified Window as W
import qualified Math.OpenGL as GL
import Math.Vector.Types
import Unsafe.Coerce




-- Apply projection matrix.
applyProjection :: W.Window -> V.View -> IO ()

-- Apply orthographic projection.
applyProjection window view@(V.Orthographic {}) = do
        GL.matrixMode $= GL.Projection
        GL.loadIdentity
        if w2h >= 1
            then GL.ortho (-w2h*dim)
                          (w2h*dim)
                          (-dim) 
                          dim
                          (-zFactor*dim)
                          (zFactor*dim)
            else GL.ortho (-dim)
                          dim
                          (-dim/w2h) 
                          (dim/w2h)
                          (-zFactor*dim)
                          (zFactor*dim)
        GL.matrixMode $= GL.Modelview 0
        GL.loadIdentity
    where
        zFactor             = unsafeCoerce  . V.zFactor     $ view
        dim                 = unsafeCoerce  . V.sceneSize   $ view
        w2h                 = unsafeCoerce  . W.aspectRatio $ window :: GL.GLdouble

-- Apply perspective projection for ThirdPerson.
applyProjection window view@(V.ThirdPerson {}) =
    applyPerspective ( V.fov view)
                     ( V.nearPlane view)
                     ( V.farPlane view)
                     ( W.aspectRatio window)

-- Apply perspective projection for FirstPerson.
applyProjection window view@(V.FirstPerson {}) =
    applyPerspective ( V.fov view)
                     ( V.nearPlane view)
                     ( V.farPlane view)
                     ( W.aspectRatio window)




-- Apply a perspective projection.
applyPerspective :: Double -> Double -> Double -> Double -> IO ()
applyPerspective fov nearPlane farPlane w2h = do
        GL.matrixMode $= GL.Projection
        GL.loadIdentity
        GL.perspective   ( unsafeCoerce fov       )
                         ( unsafeCoerce w2h       )
                         ( unsafeCoerce nearPlane )
                         ( unsafeCoerce farPlane  )
        GL.matrixMode $= GL.Modelview 0
        GL.loadIdentity




applyView :: V.View -> IO ()

applyView view@(V.Orthographic {}) = do
    eulerView (V.azimuth view) (V.elevation view) (V.tilt view)
    sceneShift . V.sceneCenter $ view

applyView view@(V.ThirdPerson {}) = do
        GL.translate (GL.Vector3 0.0 0.0 zDistance :: GL.Vector3 GL.GLdouble)
        eulerView (V.azimuth view) (V.elevation view) (V.tilt view)
        sceneShift . V.sceneCenter $ view
    where
        zDistance =  unsafeCoerce . negate . V.hoverDistance $ view

applyView (V.FirstPerson {}) = return ()



sceneShift :: Vector3 Double -> IO ()
sceneShift v = GL.translate . fmap negate $ GL.toVector3d v



eulerView :: Double -> Double -> Double -> IO ()
eulerView azimuth elevation tilt = do
    GL.rotate ( negate . unsafeCoerce  $ tilt       )
              ( GL.Vector3 0.0 0.0 1.0 :: GL.Vector3 GL.GLdouble )
    GL.rotate ( unsafeCoerce elevation )
              ( GL.Vector3 1.0 0.0 0.0 :: GL.Vector3 GL.GLdouble )
    GL.rotate ( negate . unsafeCoerce $ azimuth        )
              ( GL.Vector3 0.0 1.0 0.0 :: GL.Vector3 GL.GLdouble )
    GL.rotate (-90) (GL.Vector3 0.0 0.0 1.0 :: GL.Vector3 GL.GLdouble)
    GL.rotate (-90) (GL.Vector3 0.0 1.0 0.0 :: GL.Vector3 GL.GLdouble)


