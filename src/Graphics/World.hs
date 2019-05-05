{-# LANGUAGE PackageImports #-}

module Graphics.World
    ( FireBall(..)
    , World(..)
    --
    , createWorld
    , animateWorld
    ) where

import Linear
import Graphics.GPipe
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW

import Graphics.Geometry
import Graphics.Shader
import Graphics.RevolutionMesh

------------------------------------------------------------------------------------------------------------------------

data FireBall = FireBall
    { fireBallPosition :: !(V3 Float)
    , fireBallDirection :: !(V3 Float)
    , fireBallColor :: !(V3 Float)
    , fireBallAge :: !Double
    } deriving (Show)

------------------------------------------------------------------------------------------------------------------------

data World os = World
    { worldFireBalls :: ![FireBall]
    , worldSun :: !DirectionLight
    , worldCameras :: ![(String, Camera)]
    , worldElapsedTime :: !Double
    , worldBuffers :: ![Buffer os (B3 Float, B3 Float)]
    , worldNormalBuffers :: ![Buffer os (B3 Float)]
    }

type DurationInMs = Double

createWorld :: Window os RGBAFloat Depth -> ContextT GLFW.Handle os IO (World os)
createWorld window = do

    wallBuffer <- newBuffer (length walls)
    writeBuffer wallBuffer 0 walls

    let sphere' = map (\(V3 x y z, n) -> (V3 (x) (y) (z + 6), n)) (sphere 4)
    sphereBuffer <- newBuffer (length sphere')
    writeBuffer sphereBuffer 0 sphere'

    incalBuffer <- newBuffer (length incal)
    writeBuffer incalBuffer 0 incal

    {-
    let cylinder = uncurry zip $ generateMeshFromProfile 12 profile1
    cylinderBuffer <- newBuffer (length cylinder)
    writeBuffer cylinderBuffer 0 cylinder
    -}

    let
        createNormalBuffer :: [(V3 Float, V3 Float)] -> ContextT GLFW.Handle os IO (Buffer os (B3 Float))
        createNormalBuffer mesh = do
            let normals = concatMap (\(p, n) -> [p, p + n]) mesh
            normalBuffer <- newBuffer (length normals)
            writeBuffer normalBuffer 0 normals
            return normalBuffer

    incalNormalBuffer <- createNormalBuffer incal

    return $ World
        []
        (DirectionLight (V3 1.0 0.8 0.3 ^* 0.8) (- signorm (V3 20 5 15)) 0.1)
        [("first-camera", Camera (V3 10 10 10) (pi/4) (pi*5/4) (pi/3))]
        0
        [incalBuffer, wallBuffer, sphereBuffer]
        []

animateWorld :: DurationInMs -> World os -> ContextT GLFW.Handle os IO (World os)
animateWorld timeDelta world = return world' where

    sun = worldSun world
    sun' = sun
        { directionLightDirection = Linear.rotate
            (axisAngle (V3 0 0 1) (realToFrac timeDelta / 4))
            (directionLightDirection sun)
        }

    elapsedTime = worldElapsedTime world + timeDelta

    world' = world
        { worldSun = sun'
        , worldElapsedTime = elapsedTime
        }

------------------------------------------------------------------------------------------------------------------------

sphere :: Floating a => Int -> [(V3 a, V3 a)]
sphere depth = zip vertices vertices where
    x = 0.525731112119133606
    z = 0.850650808352039932

    rootVertices = map (\(x, y, z) -> V3 x y z)
        [ (-x, 0, z), (x, 0, z), (-x, 0, -z), (x, 0, -z)
        , (0, z, x), (0, z, -x), (0, -z, x), (0, -z, -x)
        , (z, x, 0), (-z, x, 0), (z, -x, 0), (-z, -x , 0)
        ]

    indices =
        [ (1, 4, 0), (4, 9, 0), (4, 5, 9), (8, 5, 4), (1, 8, 4)
        , (1, 10, 8), (10, 3, 8), (8, 3, 5), (3, 2, 5), (3, 7, 2)
        , (3, 10, 7), (10, 6, 7), (6, 11, 7), (6, 0, 11), (6, 1, 0)
        , (10, 1, 6), (11, 0, 9), (2, 11, 9), (5, 2, 9), (11, 2, 7)
        ]

    divide 0 (p1, p2, p3) = map signorm [p1, p2, p3]
    divide depth (p_1, p_2, p_3) =
        let p12 = p_1 + p_2
            p23 = p_2 + p_3
            p31 = p_3 + p_1
        in  concatMap (divide (depth - 1))
            [ (p_1, p12, p31)
            , (p12, p_2, p23)
            , (p31, p23, p_3)
            , (p12, p23, p31)
            ]

    getTriangle (i, j, k) = (rootVertices !! i, rootVertices !! j, rootVertices !! k)
    vertices = concatMap (divide depth . getTriangle) indices

walls :: Floating a => [(V3 a, V3 a)]
walls = zip vertices (extractNormals vertices) where
    (p0, p1, p2, p3, p4, p5, p6) = (V3 0 0 0, V3 1 0 0, V3 0 1 0, V3 0 0 1, V3 1 1 0, V3 0 1 1, V3 1 0 1)
    vertices = map (\x -> x - V3 10 10 10) $ map (^* 20) $
        [ p0, p1, p2
        , p1, p4, p2
        , p0, p2, p3
        , p2, p5, p3
        , p0, p3, p1
        , p1, p3, p6
        ]

incal :: [(V3 Float, V3 Float)]
incal = zip vertices (extractNormals vertices) where
    (pO, pA, pB, pC, pD) = (V3 0 0 2, V3 2 2 (-1), V3 (-2) 2 (-1), V3 (-2) (-2) (-1), V3 2 (-2) (-1))
    pyramid =
        [ pA, pD, pB
        , pC, pB, pD
        , pA, pB, pO
        , pB, pC, pO
        , pC, pD, pO
        , pD, pA, pO
        ]
    vertices = map (^* 2) $ pyramid ++ map (rotate (axisAngle (V3 1 0 0) pi)) pyramid

calculateTriangleNormal :: Floating a => V3 a -> V3 a -> V3 a -> V3 a
calculateTriangleNormal p1 p2 p3 = signorm $ cross (p2 ^-^ p1) (p3 ^-^ p1)

extractNormals :: Floating a => [V3 a] -> [V3 a]
extractNormals [] = []
extractNormals (p1:p2:p3:ps) = let n = calculateTriangleNormal p1 p2 p3 in [n, n, n] ++ extractNormals ps
