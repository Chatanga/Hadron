module Graphics.Intersection
    ( IntersectionResult(..)
    , pointToPlaneDistance
    , sphereWithCameraFrustumIntersect
    , sphereWithFrustumIntersect
    ) where

import Control.Lens
import Data.List
import Linear
import Data.Ord

import Graphics.Geometry

----------------------------------------------------------------------------------------------------------------------

data IntersectionResult = Inside | Outside | Intersect deriving (Show, Eq)

pointToPlaneDistance :: V4 Float -> V4 Float -> Float
pointToPlaneDistance a plan = a `dot` plan / norm (plan ^. _xyz)

sphereWithCameraFrustumIntersect :: V2 Int -> Camera -> V3 Float -> Float -> IntersectionResult
sphereWithCameraFrustumIntersect (V2 w h) camera = sphereWithFrustumIntersect frustumCorners where

    cameraPos = cameraPosition camera
    cameraMat = lookAt cameraPos (cameraPos + getSight camera) (getUp camera)

    projectionMat = perspective (cameraFov camera) (fromIntegral w / fromIntegral h) (cameraNear camera) (cameraFar camera)
    modelViewProj = projectionMat !*! cameraMat
    invModelViewProj = inv44 modelViewProj

    -- Frustum corners in normalized device coordinates.
    ndcFrustumCorners =
        [ V3 (-1) (-1) (-1)
        , V3 1 (-1) (-1)
        , V3 (-1) 1 (-1)
        , V3 1 1 (-1)
        , V3 (-1) (-1) 1
        , V3 1 (-1) 1
        , V3 (-1) 1 1
        , V3 1 1 1
        ]

    -- Same things in world coordinates.
    frustumCorners = map (normalizePoint . (invModelViewProj !*) . point) ndcFrustumCorners

sphereWithFrustumIntersect :: [V3 Float] -> V3 Float -> Float -> IntersectionResult
sphereWithFrustumIntersect frustumCorners spherePosition sphereRadius = intersection where

    toPlane i j k =
        let p1 = frustumCorners !! i
            p2 = frustumCorners !! j
            p3 = frustumCorners !! k
            V3 a b c = normalize ((p2 - p1) `cross` (p3 - p1))
            d = - pointToPlaneDistance (point p1) (V4 a b c 0)
        in  V4 a b c d

    -- Plane equations.
    nearPlane = toPlane 0 1 3
    farPlane = toPlane 4 6 7
    upPlane = toPlane 2 3 7
    downPlane = toPlane 0 4 5
    leftPlane = toPlane 0 2 6
    rightPlane = toPlane 3 1 5

    -- octants: frustum corner -> the 3 planes meeting at this corner
    octants :: [(V3 Float, [V4 Float])]
    octants = zip frustumCorners
        [ [nearPlane, downPlane, leftPlane]
        , [nearPlane, downPlane, rightPlane]
        , [nearPlane, upPlane, leftPlane]
        , [nearPlane, upPlane, rightPlane]
        , [farPlane, downPlane, leftPlane]
        , [farPlane, downPlane, rightPlane]
        , [farPlane, upPlane, leftPlane]
        , [farPlane, upPlane, rightPlane]
        ]

    -- Nearest octant.
    planes = snd (minimumBy (comparing (quadrance . (-) spherePosition . fst)) octants)

    distances = map (pointToPlaneDistance (point spherePosition)) planes

    intersection
        | any (> sphereRadius) distances = Outside
        | maximum distances < -sphereRadius = Inside -- Could also intersect if the sphere is too big
        | otherwise = Intersect
