module Graphics.Intersection
    ( IntersectionResult(..)
    , pointToPlaneDistance
    , sphereWithCameraFrustumIntersect
    , sphereWithFrustumIntersect
    , rayWithCubeIntersect
    , getRayCoordinate
    ) where

import Control.Lens
import Data.List
import Data.Maybe
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

{-
-- Note: ray direction is normalized
rayWithCubeIntersect :: (V3 Float, V3 Float) -> (Float, V3 Float) -> Maybe Float
rayWithCubeIntersect (orig, dir) (scale, lowerCorner) = if intersection then Just d else Nothing where
    halfScale = scale / 2
    center = lowerCorner + pure halfScale
    vertices = [ lowerCorner + V3 dx dy dz | dx <- [0, scale], dy <- [0, scale], dz <- [0, scale] ]
    nearestPointAlongTheRayTo p = orig + ((p - orig) `dot` dir) *^ dir
    d = (center - orig) `dot` dir
    inside p = let V3 dx dy dz = p - center in all ((<= halfScale) . abs) [dx, dy, dz]
    intersection = any (inside . nearestPointAlongTheRayTo) vertices
-}

rayWithCubeIntersect :: (Floating a, Ord a) => (V3 a, V3 a) -> (a, V3 a) -> Maybe a
rayWithCubeIntersect ray (scale, lowerCorner) =
    let minBox = lowerCorner
        maxBox = lowerCorner + pure scale
    in  getRayCoordinate ray <$> hitBoundingBox (minBox, maxBox) ray

{-
p = orig + dir ^* (getRayCoordinate (orig, dir) p)
-}
getRayCoordinate :: (Floating a, Ord a) => (V3 a, V3 a) -> V3 a -> a
getRayCoordinate (orig, dir) p = (p - orig) `dot` dir

data Location = LocRight | LocLeft | LocMiddle deriving Eq

{-
Fast Ray-Box Intersection
by Andrew Woo
from "Graphics Gems", Academic Press, 1990
-}
hitBoundingBox :: (Floating a, Ord a) => (V3 a, V3 a) -> (V3 a, V3 a) -> Maybe (V3 a)
hitBoundingBox box ray = fromList <$> hitPoint where
    toList (V3 x y z) = [x, y, z]
    fromList [x, y, z] = V3 x y z
    fromList _ = undefined

    minBox = toList (fst box)
    maxBox = toList (snd box)
    origin = toList (fst ray)
    direction = toList (snd ray)

    -- Find candidate planes; this loop can be avoided if rays cast all from the eye (assume perpsective view)
    findCandidatePlane o min max
        | o < min = (LocLeft, min, False)
        | o > max = (LocRight, max, False)
        | otherwise = (LocMiddle, undefined, True)

    (quadrant, candidatePlane, inside) = unzip3 $ zipWith3 findCandidatePlane origin minBox maxBox

    -- Ray origin inside bounding box
    hitPoint = if and inside
        then Just origin
        else coord where
            -- Calculate T distances to candidate planes
            distanceToCandidatePlane q cp o d = if q /= LocMiddle && d /= 0
                then (cp - o) / d
                else -1
            indexedMaxT = zip [0..] (zipWith4 distanceToCandidatePlane quadrant candidatePlane origin direction)

            -- Get largest of the maxT's for final choice of intersection
            (whichPlane, maxT_whichPlane) = maximumBy (comparing snd) indexedMaxT

            -- Check final candidate actually inside box
            filterCandidate i o d min max cp =
                let c = o + maxT_whichPlane * d;
                in  if whichPlane /= i
                    then if c < min || c > max
                        then Nothing
                        else Just (o + maxT_whichPlane * d)
                    else
                        Just cp

            coord = if maxT_whichPlane < 0
                then Nothing
                else sequence $ zipWith6 filterCandidate [0..] origin direction minBox maxBox candidatePlane
