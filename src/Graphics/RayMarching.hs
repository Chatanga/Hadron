{-# language ScopedTypeVariables, TypeFamilies, FlexibleContexts, AllowAmbiguousTypes #-}

module Graphics.RayMarching
    ( createRayMarchingRenderer
    )
where

import Prelude hiding ((.), id, (<*))
import Control.Category (Category((.)), id)
import Control.Lens ((&), (<&>), (^.), index)
import Control.Monad ( when )
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Exception
import Data.Foldable
import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW

import qualified Common.Random as Random

import Graphics.Color
import Graphics.Geometry
import Graphics.Shaders

----------------------------------------------------------------------------------------------------------------------

{-
GPipe translation of:
https://iquilezles.org/articles/raymarchingdf
-}

----------------------------------------------------------------------------------------------------------------------

dot2 :: (Metric f, Num a) => f a -> a
dot2 v = dot v v

ndot :: Num a => V2 a -> V2 a -> a
ndot (V2 ax ay) (V2 bx by) = ax * bx - ay * by

-- Sphere - exact (https://www.shadertoy.com/view/Xds3zN)
sdSphere :: Metric f => f FFloat -> FFloat -> FFloat
sdSphere p s = norm p - s

-- Box - exact (Youtube Tutorial with derivation: https://www.youtube.com/watch?v=62-pRVZuS5c)
sdBox :: V3 FFloat -> V3 FFloat -> FFloat
sdBox p b =
    let q = abs p - b
    in  norm (maxB 0 <$> q) + minB 0 (foldl1 maxB q)

-- Round Box - exact
sdRoundBox :: V3 FFloat -> V3 FFloat -> FFloat -> FFloat
sdRoundBox p b r =
    let q = abs p - b
    in  norm (maxB 0 <$> q) + minB 0 (foldl1 maxB q) - r

-- Box Frame - exact (https://www.shadertoy.com/view/3ljcRh)
sdBoxFrame :: V3 FFloat -> V3 FFloat -> V3 FFloat -> FFloat
sdBoxFrame p b e =
    let p'@(V3 px py pz) = abs p - b
        (V3 qx qy qz) = abs (p' + e) - e
        f x = norm (maxB 0 <$> x) + minB 0 (foldl1 maxB x)
    in  minimumB
            [ f (V3 px qy qz)
            , f (V3 qx py qz)
            , f (V3 qx qy pz)
            ]

-- Torus - exact
-- The torus axis is Y
-- tx: torus radius
-- ty: torus section radius
sdTorus :: V3 FFloat -> V2 FFloat -> FFloat
sdTorus (V3 px py pz) (V2 tx ty) = norm (V2 (norm (V2 px pz) - tx) py) - ty

-- Capped Torus - exact (https://www.shadertoy.com/view/tl23RK)
sdCappedTorus :: V3 FFloat -> V2 FFloat -> FFloat -> FFloat -> FFloat
sdCappedTorus p@(V3 px py pz) sc@(V2 scx scy) ra rb =
    let p'@(V3 px' py' pz') = V3 (abs px) py pz
        p'xy = p'^._xy
        k = ifThenElse' (scy * px' >* scx * py') (dot p'xy sc) (norm p'xy)
    in  sqrt (dot p' p' + ra * ra - 2 * ra * k) - rb

-- Link - exact (https://www.shadertoy.com/view/wlXSD7)
sdLink :: V3 FFloat -> FFloat -> FFloat -> FFloat -> FFloat
sdLink p@(V3 px py pz) le r1 r2 =
    let q = V3 px (maxB (abs py - le) 0) pz
    in  norm (V2 (norm (q^._xy) - r1) (q ^._z)) - r2

-- Infinite Cylinder - exact
sdCylinder :: V3 FFloat -> V3 FFloat -> FFloat
sdCylinder (V3 px py pz) (V3 cx cy cz) = norm (V2 px pz - V2 cx cy) - cz

-- Cone - exact
sdCone :: V3 FFloat -> V2 FFloat -> FFloat -> FFloat
sdCone p@(V3 px py pz) c@(V2 cx cy) h =
    let -- c is the sin/cos of the angle, h is height
        -- Alternatively pass q instead of (c,h),
        -- which is the point at the base in 2D
        q@(V2 qx qy) = h *^ V2 (cx / cy) (-1.0)
        w@(V2 wx wy) = V2 (norm (V2 px pz)) py
        a = w - q ^* clamp (dot w q / dot q q) 0 1
        b = w - q * V2 (clamp (wx / qx) 0 1) 1
        k = signum qy
        d = minB (dot a a) (dot b b)
        s = maxB (k * (wx * qy - wy * qx)) (k * (wy - qy))
    in  sqrt d * signum s

-- Cone - bound (not exact!)
sdBoundCone :: V3 FFloat -> V2 FFloat -> FFloat -> FFloat
sdBoundCone p@(V3 px py pz) c h =
    let q = norm (V2 px pz)
    in  maxB (dot c (V2 q py)) (-h - py)

-- Infinite Cone - exact
sdInfiniteCone :: V3 FFloat -> V2 FFloat -> FFloat
sdInfiniteCone p@(V3 px py pz) c@(V2 cx cy) =
    let -- c is the sin/cos of the angle
        q@(V2 qx qy) = V2 (norm (V2 px pz)) (-py)
        d = norm (q - c ^* maxB (dot q c) 0)
    in  d * ifThenElse' (qx * cy - qy * cx <* 0) (-1) 1

-- Plane - exact
sdPlane :: V3 FFloat -> V3 FFloat -> FFloat -> FFloat
sdPlane p n h =
    -- n must be normalized
    dot p n  + h

-- Hexagonal Prism - exact
sdHexPrism :: V3 FFloat -> V2 FFloat -> FFloat
sdHexPrism p' (V2 hx hy) =
    let splitXY_Z (V3 x y z) = (V2 x y, z)
        (kxy, kz) = splitXY_Z (V3 (-0.8660254) 0.5 0.57735)
        (p'xy, pz) = splitXY_Z (abs p')
        pxy@(V2 px py) = p'xy - 2 * minB (dot kxy p'xy) 0 *^ kxy
        d@(V2 dx dy) = V2
            (norm (V2 px py - V2 (clamp px (-kz * hx) (kz * hx)) hx) * signum (py - hx))
            (pz - hy)
    in  minB (maxB dx dy) 0 + norm (maxB 0 <$> d)

-- Triangular Prism - bound
sdTriPrism :: V3 FFloat -> V2 FFloat -> FFloat
sdTriPrism p@(V3 px py pz) (V2 hx hy) =
    let q@(V3 qx qy qz) = abs p
    in  maxB (qz - hy) (maxB (qx * 0.866025 + py * 0.5) (-py) - hx * 0.5)

-- Capsule / Line - exact
sdCapsule :: V3 FFloat -> V3 FFloat -> V3 FFloat -> FFloat -> FFloat
sdCapsule p a b r =
    let pa = p - a
        ba = b - a
        h = clamp (dot pa ba / dot ba ba) 0 1
    in  norm (pa - ba ^* h) - r

-- Capsule / Line - exact
sdVerticalCapsule :: V3 FFloat -> FFloat -> FFloat -> FFloat
sdVerticalCapsule (V3 px py pz) h r =
    let p = V3 px (py - clamp py 0 h) pz
    in  norm p  - r

-- Vertical Capped Cylinder - exact   (https://www.shadertoy.com/view/wdXGDr)
sdCappedCylinder :: V3 FFloat -> FFloat -> FFloat -> FFloat
sdCappedCylinder p h r =
    let d = abs (V2 (norm (p^._xz)) (p^._y)) - V2 r h
    in  minB (maxB (d^._x) (d^._y)) 0 + norm (maxB 0 <$> d)

-- Arbitrary Capped Cylinder - exact   (https://www.shadertoy.com/view/wdXGDr)
sdArbitraryCappedCylinder :: V3 FFloat -> V3 FFloat -> V3 FFloat -> FFloat -> FFloat
sdArbitraryCappedCylinder p a b r =
    let ba = b - a
        pa = p - a
        baba = dot ba ba
        paba = dot pa ba
        x = norm (pa ^* baba - ba ^* paba) - r * baba
        y = abs (paba - baba * 0.5) - baba * 0.5
        x2 = x * x
        y2 = y * y * baba
        d = ifThenElse' (maxB x y <* 0)
                (minB x2 y2)
                (ifThenElse' (x >* 0) x2 0 + ifThenElse' (y >* 0) y2 0)
    in  signum d * sqrt (abs d) / baba

-- Rounded Cylinder - exact
sdRoundedCylinder :: V3 FFloat -> FFloat -> FFloat -> FFloat -> FFloat
sdRoundedCylinder p ra rb h =
    let splitXZ_Y (V3 x y z) = (V2 x z, y)
        (pxz, py) = splitXZ_Y p
        d@(V2 dx dy) = V2 (norm pxz - 2 * ra + rb) (abs py - h)
    in  minB (maxB dx dy) 0 + norm (maxB 0 <$> d) - rb

-- Capped Cone - exact
sdCappedCone :: V3 FFloat -> FFloat -> FFloat -> FFloat -> FFloat
sdCappedCone p h r1 r2 =
    let splitXZ_Y (V3 x y z) = (V2 x z, y)
        (pxz, py) = splitXZ_Y p
        q@(V2 qx qy) = V2 (norm pxz) py
        k1 = V2 r2 h
        k2 = V2 (r2 - r1) (2 * h)
        ca@(V2 cax cay) = V2 (qx - minB qx (ifThenElse' (qy <* 0.0) r1 r2)) (abs qy - h)
        cb@(V2 cbx cby) = q - k1 + k2 ^* clamp (dot (k1 - q) k2 / dot2 k2) 0 1
        s = ifThenElse' (cbx <* 0 &&* cay <* 0) (-1) 1
    in  s * sqrt (minB (dot2 ca) (dot2 cb))

-- Capped Cone - exact (https://www.shadertoy.com/view/tsSXzK)
sdCappedConeBis :: V3 FFloat -> V3 FFloat -> V3 FFloat -> FFloat -> FFloat -> FFloat
sdCappedConeBis p a b ra rb =
    let rba  = rb - ra
        baba = dot (b - a) (b - a)
        papa = dot (p - a) (p - a)
        paba = dot (p - a) (b - a) / baba
        x = sqrt (papa - paba * paba * baba)
        cax = maxB 0 (x - ifThenElse' (paba <* 0.5) ra rb)
        cay = abs (paba - 0.5) - 0.5
        k = rba * rba + baba
        f = clamp ((rba * (x - ra) + paba * baba) / k) 0 1
        cbx = x - ra - f * rba
        cby = paba - f
        s = ifThenElse' (cbx <* 0 &&* cay <* 0) (-1) 1
    in  s * sqrt (minB (cax * cax + cay * cay * baba) (cbx * cbx + cby * cby * baba))

-- Solid Angle - exact (https://www.shadertoy.com/view/wtjSDW)
sdSolidAngle :: V3 FFloat -> V2 FFloat -> FFloat -> FFloat
sdSolidAngle p c@(V2 cx cy) ra =
    -- c is the sin/cos of the angle
    let splitXZ_Y (V3 x y z) = (V2 x z, y)
        (pxz, py) = splitXZ_Y p
        q@(V2 qx qy) = V2 (norm pxz) py
        l = norm q - ra
        m = norm (q - c ^* clamp (dot q c) 0 ra)
    in  maxB l (m * signum (cy * qx - cx * qy))

-- Cut Sphere - exact (https://www.shadertoy.com/view/stKSzc)
sdCutSphere :: V3 FFloat -> FFloat -> FFloat -> FFloat
sdCutSphere p r h =
    let splitXZ_Y (V3 x y z) = (V2 x z, y)
        (pxz, py) = splitXZ_Y p
        -- sampling independent computations (only depend on shape)
        w = sqrt (r * r - h * h)
        -- sampling dependant computations
        q@(V2 qx qy) = V2 (norm pxz) py
        s = maxB ((h - r) * qx * qx + w * w * (h + r - 2 * qy)) (h * qx - w * qy)
    in  ifThenElse' (s <* 0.0)
            (norm q - r)
            (ifThenElse' (qx <* w) (h - qy) (norm (q - V2 w h)))

-- Cut Hollow Sphere - exact (https://www.shadertoy.com/view/7tVXRt)
sdCutHollowSphere :: V3 FFloat -> FFloat -> FFloat -> FFloat -> FFloat
sdCutHollowSphere p r h t =
    let splitXZ_Y (V3 x y z) = (V2 x z, y)
        (pxz, py) = splitXZ_Y p
        -- sampling independent computations (only depend on shape)
        w = sqrt (r * r - h * h)
        -- sampling dependant computations
        q@(V2 qx qy) = V2 (norm pxz) py
    in  ifThenElse' (h * qx <* w * qy)
            (norm (q - V2 w h))
            (abs (norm q - r) - t)

-- Death Star - exact (https://www.shadertoy.com/view/7lVXRt)
sdDeathStar :: V3 FFloat -> FFloat -> FFloat -> FFloat -> FFloat
sdDeathStar p2 ra rb d =
    let splitX_YZ (V3 x y z) = (x, V2 y z)
        (p2x, p2yz) = splitX_YZ p2
        -- sampling independent computations (only depend on shape)
        a = (ra * ra - rb * rb + d * d) / (2 * d)
        b = sqrt (maxB (ra * ra - a * a) 0)
        -- sampling dependant computations
        p@(V2 px py) = V2 p2x (norm p2yz)
    in  ifThenElse' (px * b - py * a >* d * maxB (b - py) 0)
            (norm (p - V2 a b))
            (maxB (norm p - ra) (-(norm (p - V2 d 0) - rb)))

-- Round cone - exact
sdRoundCone :: V3 FFloat -> FFloat -> FFloat -> FFloat -> FFloat
sdRoundCone p r1 r2 h =
    let splitXZ_Y (V3 x y z) = (V2 x z, y)
        (pxz, py) = splitXZ_Y p
        -- sampling independent computations (only depend on shape)
        b = (r1 - r2) / h
        a = sqrt (1 - b * b)
        -- sampling dependant computations
        q = V2 (norm pxz) py
        k = dot q (V2 (-b) a)
    in  guardedB undefined
            [ (k <* 0, norm q - r1)
            , (k >* a * h, norm (q - V2 0 h) - r2)
            ] (dot q (V2 a b) - r1)

-- Round Cone - exact (https://www.shadertoy.com/view/tdXGWr)
sdRoundConeBis :: V3 FFloat -> V3 FFloat -> V3 FFloat -> FFloat -> FFloat -> FFloat
sdRoundConeBis p a b r1 r2 =
    let -- sampling independent computations (only depend on shape)
        ba = b - a
        l2 = dot ba ba
        rr = r1 - r2
        a2 = l2 - rr * rr
        il2 = 1 / l2
        -- sampling dependant computations
        pa = p - a
        y = dot pa ba
        z = y - l2
        x2 = dot2 (pa ^* l2 - ba ^* y)
        y2 = y * y * l2
        z2 = z * z * l2
        -- single square root!
        k = signum rr * rr * rr * x2
    in  guardedB undefined
            [ (signum z * a2 * z2 >* k, sqrt (x2 + z2) * il2 - r2)
            , (signum y * a2 * y2 <* k, sqrt (x2 + y2) * il2 - r1)
            ] ((sqrt (x2 * a2 * il2) + y * rr) * il2 - r1)

-- Ellipsoid - bound (not exact!) (https://www.shadertoy.com/view/tdS3DG)
sdEllipsoid :: V3 FFloat -> V3 FFloat -> FFloat
sdEllipsoid p r =
    let k0 = norm (p / r)
        k1 = norm (p / (r * r))
    in  k0 * (k0 - 1) / k1

-- Rhombus - exact (https://www.shadertoy.com/view/tlVGDc)
sdRhombus :: V3 FFloat -> FFloat -> FFloat -> FFloat -> FFloat -> FFloat
sdRhombus p la lb h ra =
    let V3 px py pz = abs p
        pxz = V2 px pz
        b@(V2 bx by) = V2 la lb
        f = clamp (ndot b (b - 2 * pxz) / dot b b) (-1) 1
        q@(V2 qx qy) = V2 (norm (pxz - 0.5 * b * V2 (1 - f) (1 + f)) * signum (px * by + pz * bx - bx * by) - ra) (py - h)
    in  minB (maxB qx qy) 0 + norm (maxB 0 <$> q)

-- Octahedron - exact (https://www.shadertoy.com/view/wsSGDG)
sdOctahedron :: V3 FFloat -> FFloat -> FFloat
sdOctahedron p s =
    let V3 px py pz = abs p
        m = px + py + pz - s
        f (V3 qx qy qz) =
            let k = clamp (0.5 * (qz - qy + s)) 0 s
            in  norm (V3 qx (qy - s + k) (qz - k))
    in  guardedB undefined
            [ (3 * px <* m, f (p^._xyz))
            , (3 * py <* m, f (p^._yzx))
            , (3 * pz <* m, f (p^._zxy))
            ] (m * 0.57735027)

-- Octahedron - bound (not exact)
sdOctahedronBis :: V3 FFloat -> FFloat -> FFloat
sdOctahedronBis p s =
    let V3 px py pz = abs p
    in  (px + py + pz - s) * 0.57735027

-- Pyramid - exact   (https://www.shadertoy.com/view/Ws3SDl)
sdPyramid :: V3 FFloat -> FFloat -> FFloat
sdPyramid p h =
    let m2 = h * h + 0.25

        py = p^._y
        p'@(V2 px' pz') = abs (p^._xz)
        V2 px pz = ifThenElse' (pz' >* px') (V2 pz' px') (V2 px' pz') - pure 0.5

        q@(V3 qx qy qz) = V3 pz (h * py - 0.5 * px) (h * px + 0.5 * py)

        s = maxB (-qx) 0
        t = clamp ((qy - 0.5 * pz) / (m2 + 0.25)) 0 1

        a = m2 * (qx + s) * (qx + s) + qy * qy
        b = m2 * (qx + 0.5 *t) * (qx + 0.5 * t) + (qy - m2 * t) * (qy - m2 * t)

        d2 = ifThenElse' (minB qy (-qx * m2 - qy * 0.5) >* 0) 0 (minB a b)

    in  sqrt ((d2 + qz * qz) / m2) * signum (maxB qz (-py))

-- Triangle - exact (https://www.shadertoy.com/view/4sXXRN)
udTriangle :: V3 FFloat -> V3 FFloat -> V3 FFloat -> V3 FFloat -> FFloat
udTriangle p a b c =
    let ba = b - a
        pa = p - a
        cb = c - b
        pb = p - b
        ac = a - c
        pc = p - c
        nor = cross ba ac

    in  sqrt $ ifThenElse'
            ( signum (dot (cross ba nor) pa)
            + signum (dot (cross cb nor) pb)
            + signum (dot (cross ac nor) pc) <* 2)
            ( minimumB
                [ dot2 (ba ^* clamp (dot ba pa / dot2 ba) 0 1 - pa)
                , dot2 (cb ^* clamp (dot cb pb / dot2 cb) 0 1 - pb)
                , dot2 (ac ^* clamp (dot ac pc / dot2 ac) 0 1 - pc) ])
            (dot nor pa * dot nor pa / dot2 nor)

-- Quad - exact (https://www.shadertoy.com/view/Md2BWW)
udQuad :: V3 FFloat -> V3 FFloat -> V3 FFloat -> V3 FFloat -> V3 FFloat -> FFloat
udQuad p a b c d =
    let ba = b - a
        pa = p - a
        cb = c - b
        pb = p - b
        dc = d - c
        pc = p - c
        ad = a - d
        pd = p - d
        nor = cross ba ad

    in  sqrt $ ifThenElse'
            ( signum (dot (cross ba nor) pa)
            + signum (dot (cross cb nor) pb)
            + signum (dot (cross dc nor) pc)
            + signum (dot (cross ad nor) pd) <* 3)
            (minimumB
                [ dot2 (ba ^* clamp (dot ba pa / dot2 ba) 0 1 - pa)
                , dot2 (cb ^* clamp (dot cb pb / dot2 cb) 0 1 - pb)
                , dot2 (dc ^* clamp (dot dc pc / dot2 dc) 0 1 - pc)
                , dot2 (ad ^* clamp (dot ad pd / dot2 ad) 0 1 - pd) ])
            (dot nor pa * dot nor pa / dot2 nor)

-- ***

class KindOfLinear a where
    interpolate :: FFloat -> a -> a -> a

{-
smoothMin :: KindOfLinear a => FFloat -> (FFloat, a) -> (FFloat, a) -> (FFloat, a)
smoothMin k (dstA, vA) (dstB, vB) =
    let h = maxB (k - abs (dstA - dstB)) 0 / k
    in  minB dstA dstB - h * h * h * h / 6

smoothMinimum ::  KindOfLinear a => FFloat -> [(FFloat, a)] -> (FFloat, a)
smoothMinimum k = foldl1 (smoothMin k)
-}

sceneDistance' :: V3 FFloat -> (FFloat, V3 FFloat)
sceneDistance' p = (f p, red) where
    -- sdSphere p 5
    -- sdBox p (V3 2 4 6)
    -- sdRoundBox p (V3 2 4 6) 1
    -- sdBoxFrame p (V3 2 4 6) (V3 0.1 0.2 0.3)
    -- sdTorus p (V2 5 1)
    -- sdCappedTorus p (V2 (sin 1) (cos 1)) 5 1
    -- sdLink p 2 5 1
    -- sdCylinder p (V3 2 4 6)
    -- sdCone p (V2 (sin 0.5) (cos 0.5)) 5 
    -- sdBoundCone p (V2 (sin 0.5) (cos 0.5)) 5
    -- sdInfiniteCone p (V2 (sin 0.5) (cos 0.5))
    -- sdPlane p (signorm $ V3 2 1 5) 3
    -- sdHexPrism p (V2 2 4)
    -- sdTriPrism p (V2 4 2)
    f p = sdCapsule p (V3 2 4 6) (V3 1 2 3) 3
    -- sdVerticalCapsule
    -- sdCappedCylinder
    -- sdArbitraryCappedCylinder
    -- sdRoundedCylinder
    -- sdCappedCone
    -- sdCappedConeBis
    -- sdSolidAngle
    -- sdCutSphere
    -- sdCutHollowSphere
    -- sdDeathStar
    -- sdRoundCone
    -- sdRoundConeBis
    -- sdEllipsoid
    -- sdRhombus
    -- sdOctahedron
    -- sdOctahedronBis
    -- sdPyramid

sceneDistance'' :: V3 FFloat -> (FFloat, V3 FFloat)
sceneDistance'' p =
    let p' = p -- mod'' p (pure 40)
        sphere center radius = norm (p' - center) - radius
        box center size = let V3 x y z = (abs (p' - center) - size) in maximumB [x, y, z]
    in  minimumByB (comparingB fst)
            [ (sphere (V3 10 10 10) 5, red)
            , (sphere (V3 20 10 10) 6, blue)
            , (box (V3 10 14 10) (V3 2 4 6), green)
            ]

sceneDistance :: V3 FFloat -> FFloat
sceneDistance = fst . sceneDistance'

-- ***

fastNormal :: V3 FFloat -> V3 FFloat
fastNormal x =
    let e = 1e-4
        u = V3 e 0 0
        v = V3 0 e 0
        w = V3 0 0 e
    in  signorm (V3
            (sceneDistance (x + u))
            (sceneDistance (x + v))
            (sceneDistance (x + w)) - pure (sceneDistance x))

normal :: V3 FFloat -> V3 FFloat
normal x =
    let e = 1e-4
        u = V3 e 0 0
        v = V3 0 e 0
        w = V3 0 0 e
    in  signorm (V3
            (sceneDistance (x + u) - sceneDistance (x - u))
            (sceneDistance (x + v) - sceneDistance (x - v))
            (sceneDistance (x + w) - sceneDistance (x - w)))

getTexelColor :: DirectionLightS F -> (V3 FFloat, FFloat, FFloat, V4 (V4 FFloat)) -> V2 FFloat -> RasterizedInfo -> V4 FFloat
getTexelColor sun (eye, near, far, invModelViewProj) (V2 x y) _ = point color' where
    nearRay = rectify (invModelViewProj !* V4 x y (-1) 1) - eye
    rectify (V4 x y z w) = V3 (x / w) (y / w) (z / w)
    slantNearDistance = norm nearRay
    slantFarDistance = far * slantNearDistance / near
    ray = nearRay ^/ slantNearDistance
    (rayMarch, color) = fst $ while
        (\((d, c), s) -> d <* slantFarDistance &&* s >* 1e-2)
        (\((d, c), s) -> let (s', c') = sceneDistance' (eye + ray ^* (d + s)) in ((d + s, c'), s'))
        ((0, black), slantNearDistance)
    color' = ifThenElse' (rayMarch <* slantFarDistance) (color ^* f) (V3 0.1 0.1 0.1)
    n = normal (eye + ray ^* rayMarch)
    f = dot (directionLightDirectionS sun) n

createScreenRenderer :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m)
    => Window os RGBAFloat Depth
    -> Buffer os (Uniform DirectionLightB)
    -> Buffer os (Uniform (B3 Float, B Float, B Float, V4 (B4 Float)))
    -> ContextT ctx os m (ViewPort -> Render os ())
createScreenRenderer window sunBuffer cameraBuffer = do
    screenBuffer :: Buffer os (B2 Float) <- newBuffer 4
    writeBuffer screenBuffer 0 [V2 (-1) (-1), V2 1 (-1), V2 1 1, V2 (-1) 1]

    shader :: CompiledShader os (ViewPort, PrimitiveArray Triangles (B2 Float))  <- compileShader $ do
        sun <- getUniform (const (sunBuffer, 0))
        camera <- getUniform (const (cameraBuffer, 0))

        triangles :: PrimitiveStream Triangles (VPos, V2 VFloat) <- toPrimitiveStream snd
            <&> fmap (\(V2 x y) -> (V4 x y 0 1, V2 x y))

        let rasterOptions = \(viewPort, _) -> (Front, viewPort, DepthRange 0 1)
        fs :: FragmentStream (V4 FFloat) <- rasterize rasterOptions triangles
            <&> withRasterizedInfo (getTexelColor sun camera)

        let colorOption = ContextColorOption NoBlending (pure True)
        drawWindowColor (const (window, colorOption)) fs

    return $ \viewPort -> do
        screen <- toPrimitiveArray TriangleFan <$> newVertexArray screenBuffer
        shader (viewPort, screen)

createGridRenderer :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m)
    => Window os RGBAFloat Depth
    -> Buffer os (Uniform (V4 (B4 Float), V4 (B4 Float), B3 Float))
    -> Buffer os (Uniform FogB)
    -> ContextT ctx os m (ViewPort -> Render os ())
createGridRenderer window projectionBuffer fogBuffer = do
    let grid =
            [ V3 1 1 0,  V3 (-1) 1 0,  V3 1 (-1) 0
            , V3 (-1) (-1) 0,  V3 1 (-1) 0,  V3 (-1) 1 0
            ]

    gridBuffer :: Buffer os (B3 Float) <- newBuffer (length grid)
    writeBuffer gridBuffer 0 grid

    shader :: CompiledShader os (ViewPort, PrimitiveArray Triangles (B3 Float))  <- compileShader $ do
        (projectionMat, cameraMat, camPos) <- getUniform (const (projectionBuffer, 0))
        let modelViewProj = projectionMat !*! cameraMat

        triangles :: PrimitiveStream Triangles (V3 VFloat) <- toPrimitiveStream snd
        let
            projectedTriangles :: PrimitiveStream Triangles (V4 VFloat, (V2 VFloat, VFloat))
            projectedTriangles =
                (\p -> (modelViewProj !* p, (p^._xy, camPos^._z))) .
                point.
                (* 4000) <$> triangles

            getRasterOptions (viewPort, _) = (FrontAndBack, viewPort, DepthRange 0 1)

        fog <- getUniform (const (fogBuffer, 0))
        fragCoord :: FragmentStream (V2 FFloat, FFloat) <- rasterize getRasterOptions projectedTriangles
        let
            gridSize = 10
            drawGridLine :: (V2 FFloat, FFloat) -> V4 FFloat
            drawGridLine (p@(V2 x y), camPosZ) = color' where
                -- Pick a coordinate to visualize in a grid.
                (coord, coord') = (p / gridSize, p / gridSize / 10)
                -- Compute anti-aliased renderContext-space grid lines.
                V2 gx gy = abs (fract' (coord - 0.5) - 0.5) / (fwidth <$> coord)
                V2 gx' gy' = abs (fract' (coord' - 0.5) - 0.5) / (fwidth <$> coord')
                -- Change color when coord' and coord' match.
                (V3 r g b) =
                    ifThenElse' (abs x <* 0.5) (V3 0 1 0) -- ortho to X = Y => green
                        (ifThenElse' (abs y <* 0.5) (V3 1 0 0) -- ortho to Y = Z => red
                            (ifThenElse' (gx >* (gx' - 0.5) ||* gy >* (gy' - 0.5))
                                (pure 0.1)
                                (pure 0.3)))
                -- Just visualize the grid lines directly.
                line = minB gx gy
                color = V4 r g b (1 - minB line 1)
                -- Add fog.
                fogDistance = norm $ V3 (p^._x) (p^._y) camPosZ
                color' = applyFog fog color (fogDistance * 0.5)

            litFrags = drawGridLine <$> fragCoord

            litFragsWithDepth = withRasterizedInfo
                (\a p -> (a, rasterizedFragCoord p ^._z)) litFrags
            blending = BlendRgbAlpha
                (FuncAdd, FuncAdd)
                (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors Zero Zero)
                (point white)
            colorOption = ContextColorOption blending (pure True)
            depthOption = DepthOption Less True

        drawWindowColorDepth (const (window, colorOption, depthOption)) litFragsWithDepth

    return $ \viewPort -> do
        gridPrimArray <- toPrimitiveArray TriangleList <$> newVertexArray gridBuffer
        shader (viewPort, gridPrimArray)

createRayMarchingRenderer :: (MonadIO m, MonadAsyncException m)
    => Window os RGBAFloat Depth
    -> ContextT GLFW.Handle os m (RenderContext m os)
createRayMarchingRenderer window = do

    fogBuffer :: Buffer os (Uniform FogB) <- newBuffer 1
    sunBuffer :: Buffer os (Uniform DirectionLightB) <- newBuffer 1
    cameraBuffer :: Buffer os (Uniform (B3 Float, B Float, B Float, V4 (B4 Float))) <- newBuffer 1
    projectionBuffer :: Buffer os (Uniform (V4 (B4 Float), V4 (B4 Float), B3 Float)) <- newBuffer 1

    screenRenderer <- createScreenRenderer window sunBuffer cameraBuffer
    gridRenderer <- createGridRenderer window projectionBuffer fogBuffer

    let renderIt _ bounds camera _ sun lights _ _ gui cursor = do
            let ((x, y), (w, h)) = bounds
                debug = guiDebug gui
                fogDensity = guiFogDensity gui

            writeBuffer fogBuffer 0 [Fog (point skyBlue) 100 1000 fogDensity]
            writeBuffer sunBuffer 0 [sun]

            let projection@(projectionMat, cameraMat, cameraPos) = createProjection bounds camera
                invModelViewProj = inv44 (projectionMat !*! cameraMat)
            writeBuffer cameraBuffer 0 [(cameraPos, {- cameraNear camera -} 1, cameraFar camera, invModelViewProj)]

            writeBuffer projectionBuffer 0 [projection]

            render $ do
                clearWindowDepth window 1
                let viewPort = ViewPort (V2 x y) (V2 w h)
                screenRenderer viewPort
                when debug $ do
                    gridRenderer viewPort

            return $ RenderContext Nothing renderIt

    return $ RenderContext Nothing renderIt
