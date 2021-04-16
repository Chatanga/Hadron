{-# LANGUAGE PackageImports #-}

module Graphics.Geometry
    ( BoundingInfo(..)
    , Camera(..)
    , getSight
    , getBearing
    , getLeft
    , getUp
    , toWorld
    , lookAt'
    --
    , noRotation
    , noTranslation
    , scaling
    , translating
    , rotating
    --
    , showV1
    , showV2
    , showV3
    , showV4
    ) where

import "lens" Control.Lens

import Data.List

import Linear
import Numeric

------------------------------------------------------------------------------------------------------------------------

data BoundingInfo
    = BoundingSphere (V3 Float) Float -- center radius
    | OrthoBoundingBox (V3 Float) (V3 Float) -- center (witdh, height, depth)

-- See https://en.wikipedia.org/wiki/Horizontal_coordinate_system
data Camera = Camera
    { cameraPosition :: !(V3 Float)
    -- The altitude angle (or elevation) in  ]-π/2, π/2[, not the distance (a bit misleading this term).
    , cameraAltitude :: !Float
    -- The azimuth in [0, 2π[ relative to X/Y/Z axis?
    , cameraAzimuth :: !Float
    -- The vertical field of view.
    , cameraFov :: !Float
    , cameraNear :: !Float
    , cameraFar :: !Float
    } deriving (Eq, Show)

getSight :: Camera -> V3 Float
getSight camera = rotate (axisAngle (getLeft camera) (cameraAltitude camera)) (getBearing camera)

getBearing :: Camera -> V3 Float
getBearing camera = V3 (cos a) (sin a) 0 where a = cameraAzimuth camera

getLeft :: Camera -> V3 Float
getLeft camera = V3 (cos a) (sin a) 0 where a = cameraAzimuth camera + pi / 2

getUp :: Camera -> V3 Float
getUp _ = V3 0 0 1

toWorld :: (Float, Float) -> (Float, Float) -> (Float, Float) -> M44 Float -> M44 Float -> V4 Float
toWorld (w, h) (x, y) (near, far) projection camera = rectify target where
    viewport = V4
        (V4 (w/2) 0 0 (w/2))
        (V4 0 (-h/2) 0 (h/2))
        (V4 0 0 (far - near) near)
        (V4 0 0 0 1)
    t = viewport !*! projection !*! camera
    target = inv44 t !* V4 x y 0 1
    rectify (V4 x y z w) = V4 (x / w) (y / w) (z / w) 1

-- Copy of lookAt from linear with normalize replaced with signorm (faster? or
-- without the epsilon constraint which is not fulfilled in shaders?).
lookAt' :: Floating a => V3 a -> V3 a -> V3 a -> V4 (V4 a)
lookAt' eye center up =
    V4 (V4 (xa^._x)  (xa^._y)  (xa^._z)  xd)
        (V4 (ya^._x)  (ya^._y)  (ya^._z)  yd)
        (V4 (-za^._x) (-za^._y) (-za^._z) zd)
        (V4 0         0         0          1)
    where
        za = signorm $ center - eye
        xa = signorm $ cross za up
        ya = cross xa za
        xd = -dot xa eye
        yd = -dot ya eye
        zd = dot za eye

------------------------------------------------------------------------------------------------------------------------

noRotation :: (Epsilon a, Floating a) => Quaternion a
noRotation = axisAngle (V3 0 1 0) 0

noTranslation :: Floating a => V3 a
noTranslation = V3 0 0 0

scaling :: Floating a => V3 a -> M44 a
scaling (V3 sx sy sz) = V4
    (V4 sx 0 0 0)
    (V4 0 sy 0 0)
    (V4 0 0 sz 0)
    (V4 0 0 0 1)

translating :: Floating a => V3 a -> M44 a
translating (V3 dx dy dz) = V4
    (V4 1 0 0 dx)
    (V4 0 1 0 dy)
    (V4 0 0 1 dz)
    (V4 0 0 0 1)

rotating :: (Epsilon a, Floating a) => V3 a -> a -> M44 a
rotating axis angle = mkTransformation (axisAngle axis angle) noTranslation

showV1 :: Real a => a -> String
showV1 x = showGFloat (Just 2) (realToFrac x) ""

showVn :: Real a => [a] -> String
showVn v = "(" ++ intercalate ", " (map showV1 v) ++ ")"

showV2 (V2 x y) = showVn [x, y]
showV3 (V3 x y z) = showVn [x, y, z]
showV4 (V4 x y z w) = showVn [x, y, z, w]
