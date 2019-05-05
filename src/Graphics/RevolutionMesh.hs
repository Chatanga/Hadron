module Graphics.RevolutionMesh
    ( ProfilePoint
    , profile1
    , profile2
    , generateMeshFromProfile
    )
where

import Data.Fixed as DF (mod')

import Graphics.GPipe

data ProfilePoint = ProfilePoint {
    pp_point :: (Float, Float),
    pp_prevNormalFactor :: Float,
    pp_nextNormalFactor :: Float,
    pp_prevTangentFactor :: Float,
    pp_nextTangentFactor :: Float
}

profile1 :: [ProfilePoint]
profile1 =
    [ ProfilePoint (4, 3) 0.0 0.0 0.5 0.5
    , ProfilePoint (-4, 1) 0.0 0.0 0.5 0.5
    , ProfilePoint (0, 1) 0.0 0.0 0.5 0.5
    , ProfilePoint (10, 0) 0.0 0.0 0.5 0.5
    , ProfilePoint (0, 3) 0.0 0.0 0.5 0.5
    , ProfilePoint (-2, 0) 0.0 0.0 0.5 0.5
    , ProfilePoint (0, 1) 0.0 0.0 0.5 0.5
    , ProfilePoint (21, 0) 0.0 0.0 0.5 0.5
    , ProfilePoint (2, -2) 0.5 0.5 0.5 0.5
    , ProfilePoint (4, 0) 0.5 0.5 0.5 0.5
    , ProfilePoint (2, -2) 0.5 0.5 0.5 0.5
    , ProfilePoint (1, -3) 0.5 0.5 0.5 0.5
    ]

profile2 :: [ProfilePoint]
profile2 =
    [ ProfilePoint (2, 2) 0.5 0.5 0.5 0.5
    , ProfilePoint (2, 2) 0.5 0.5 0.5 0.5
    , ProfilePoint (2, 0) 0.5 0.5 0.5 0.5
    , ProfilePoint (2, 0) 0.5 0.5 0.5 0.5
    , ProfilePoint (2, -2) 0.5 0.5 0.5 0.5
    ]

generateMeshFromProfile
    :: Int
    -> [ProfilePoint]
    -> ([V3 Float], [V3 Float]) -- triangles (points, normals).
generateMeshFromProfile n profile = (triangles, normals) where

    uvPoints :: [(Float, Float)]
    uvPoints = tail $ scanl(\(u, v) pp -> let (du, dv) = pp_point pp in (u+du, v+dv)) (0, 0) profile

    alphaAngles :: [Float]
    alphaAngles = let as = [a | i <- [0 .. n-1], let a = 2 * pi * (fromIntegral i + 0.5) / fromIntegral n] in as
    
    betaAngles :: [Float]
    betaAngles = pi/2 : (zipWith (\(u1, v1) (u2, v2) -> atan2 (v2 - v1) (u2 - u1)) uvPoints (tail uvPoints)) ++ [-pi/2]

    points :: [[V3 Float]] -- list of rings of points
    points =
        for uvPoints $ \(u, v) ->
            for [0 .. n-1] $ \i ->
                let
                    a = 2 * pi * fromIntegral i / fromIntegral n
                    (c, s) = (cos a, sin a)
                in V3 u (v*c) (v*s)

    triangles = concat . concat $
        for [0 .. length points - 2] $ \j ->
            for [0 .. n-1] $ \i ->
                let
                    j' = j + 1
                    i' = (i + 1) `mod` n
                    (p11, p12) = (points !! j !! i, points !! j !! i')
                    (p21, p22) = (points !! j' !! i, points !! j' !! i')
                in [p11, p22, p21, p11, p12, p22]

    normals = concat . concat $
        for [0 .. length profile - 2] $ \j ->
            for [0 .. n-1] $ \i ->
                let
                    [p1, p2] = map (profile !!) [j, j+1]
                    [b1, b2, b3] = map (betaAngles !!) [j, j+1, j+2]
                    [a1, a2, a3] = map (alphaAngles !!) [(i+n-1) `mod` n, i, (i+1) `mod` n]

                    p1a = angleLerp (pp_nextTangentFactor p1) a2 a1
                    p2a = angleLerp (pp_prevTangentFactor p2) a2 a3
                    p1b = angleLerp (pp_nextNormalFactor p1) b2 b1
                    p2b = angleLerp (pp_prevNormalFactor p2) b2 b3

                    toNormal a b = rotate (axisAngle tangent b) normal where
                        rotX = axisAngle (V3 1 0 0) a
                        normal = rotate rotX (V3 0 1 0)
                        tangent = rotate rotX (V3 0 0 1)
                in
                    [ toNormal p1a p1b
                    , toNormal p2a p2b
                    , toNormal p1a p2b
                    , toNormal p1a p1b
                    , toNormal p2a p1b
                    , toNormal p2a p2b
                    ]

for :: [a] -> (a -> b) -> [b]
for = flip fmap

angleLerp :: Float -> Float -> Float -> Float
angleLerp t a0 a1 = a0 + shortAngleDist a0 a1 * t

{- | 2D Angle Interpolation (shortest distance).
Benefits:
1. Angles do NOT need to be normalized.
2. Implementation is portable, regardless of how the modulo "%" operator outputs sign (i.e. Python, Ruby, Javascript)
3. Very easy to remember.
Thanks to Trey Wilson for the closed-form solution for shortAngleDist!
-}
shortAngleDist :: Float -- ^ 0 = start angle
    -> Float -- ^ a1 = end angle
    -> Float -- ^ t = interpolation factor (0.0=start, 1.0=end)
shortAngleDist a0 a1 = DF.mod' (2 * da) maxAngle - da where
    maxAngle = pi * 2;
    da = DF.mod' (a1 - a0) maxAngle
