{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies #-}

module Graphics.World
    ( Stage(..)
    , Renderable(..)
    , FireBall(..)
    , DirectionLight(..)
    , PointLight(..)
    , World(..)
    , ShaderEnvironment(..)
    , GridShaderEnvironment(..)
    --
    , createWorld
    , animateWorld
    ) where

import Prelude hiding ((<*))

import Control.Applicative (pure)
import "lens" Control.Lens

import Linear
import Graphics.GPipe
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW

import Graphics.Geometry
import Graphics.Shader

------------------------------------------------------------------------------------------------------------------------

data Stage
    = ShadowMappingStage
    | DirectShadingStage
    | DeferredShadingStage
    deriving (Eq)

class Renderable a where
    toRenderables :: a -> ContextT GLFW.Handle os IO [ViewPort -> Stage -> Render os ()]

data FireBall = FireBall
    { fireBallPosition :: !(V3 Float)
    , fireBallDirection :: !(V3 Float)
    , fireBallColor :: !(V3 Float)
    , fireBallAge :: !Double
    } deriving (Show)

instance Renderable FireBall where
    toRenderables (FireBall p d (V3 cx cy cz) a) = undefined

data ShadowShaderEnvironment = ShadowShaderEnvironment
    { shadowPrimitives :: PrimitiveArray Triangles (B3 Float, B3 Float)
    , shadowImage :: Image (Format Depth)
    , shadowRasterOptions :: (Side, ViewPort, DepthRange)
    }

data ShaderEnvironment = ShaderEnvironment
    { primitives :: PrimitiveArray Triangles (B3 Float, B3 Float)
    , rasterOptions :: (Side, ViewPort, DepthRange)
    }

data GridShaderEnvironment = GridShaderEnvironment
    { gridPrimitives :: PrimitiveArray Triangles (B3 Float)
    , gridRasterOptions :: (Side, ViewPort, DepthRange)
    }

-- type WorldUniform os = Buffer os (Uniform (V4 (B4 Float), V3 (B3 Float), DirectionLightB, V4 (B4 Float)))


------------------------------------------------------------------------------------------------------------------------

data World os = World
    { worldShaderConfigUniformBuffer :: Buffer os (Uniform ShaderConfigB)
    , worldPointLightUniformBuffer :: Buffer os (Uniform PointLightB)
    , worldRenderables :: ![ViewPort -> Stage -> Render os ()] -- Trop peu, pas assez.
    , worldFireBalls :: ![FireBall]
    , worldSun :: !DirectionLight
    , worldCameras :: ![(String, Camera)]
    , worldElapsedTime :: !Double
    , worldDepthTex :: (Texture2D os (Format Depth), Size2)
    , worldColorTex :: (Texture2D os (Format RGBFloat), Size2)
    }

type DurationInMs = Double

newTexture2D' :: TextureFormat c
    => Format c
    -> Size2
    -> MaxLevels
    -> ContextT GLFW.Handle os IO (Texture2D os (Format c), Size2)
newTexture2D' format size level = do
    tex <- newTexture2D format size level
    return (tex, size)

createWorld :: Window os RGBAFloat Depth -> ContextT GLFW.Handle os IO (World os)
createWorld window = do

    shaderConfigUniformBuffer :: Buffer os (Uniform ShaderConfigB) <- newBuffer 1
    pointLightUniformBuffer :: Buffer os (Uniform PointLightB) <- newBuffer 8

    gridBuffer :: Buffer os (B3 Float) <- newBuffer 6
    writeBuffer gridBuffer 0
        [ V3 1 1 0,  V3 (-1) 1 0,  V3 1 (-1) 0
        , V3 (-1) (-1) 0,  V3 1 (-1) 0,  V3 (-1) 1 0
        ]

    wallBuffer :: Buffer os (B3 Float, B3 Float) <- newBuffer (length walls)
    writeBuffer wallBuffer 0 walls

    let sphere' = map (\(V3 x y z, n) -> (V3 (x) (y) (z + 6), n)) (sphere 4)
    sphereBuffer :: Buffer os (B3 Float, B3 Float) <- newBuffer (length sphere')
    writeBuffer sphereBuffer 0 sphere'

    incalBuffer :: Buffer os (B3 Float, B3 Float) <- newBuffer (length incal)
    writeBuffer incalBuffer 0 incal

    -- Render os (PrimitiveArray Triangles (B3 Float))
    let makeGridPrimArray = toPrimitiveArray TriangleList <$> newVertexArray gridBuffer

    -- Render os (PrimitiveArray Triangles (B3 Float, B3 Float))
    -- let makePrimArrays = map (toPrimitiveArray TriangleList <$>) [ incalBuffer, wallBuffer ]
    let
        makePrimArrays :: Render os [(PrimitiveArray Triangles (B3 Float, B3 Float))]
        makePrimArrays = do
            a <- toPrimitiveArray TriangleList <$> newVertexArray incalBuffer
            b <- toPrimitiveArray TriangleList <$> newVertexArray wallBuffer
            c <- toPrimitiveArray TriangleList <$> newVertexArray sphereBuffer
            return [a, b, c]

    shadowTex <- newTexture2D' Depth16 (V2 2048 2048) 1
    colorTex <- newTexture2D' RGB8 (V2 800 600) 1

    shadowShader <- compileShader $ do
        config <- getUniform (const (shaderConfigUniformBuffer, 0))
        let viewProjMat = shaderConfigProjectionS config !*! shaderConfigCameraS config !*! shaderConfigTransformationS config

        triangles :: PrimitiveStream Triangles (V3 VFloat, V3 VFloat) <- toPrimitiveStream shadowPrimitives
        let
            projectedTriangles :: PrimitiveStream Triangles (V4 VFloat, ())
            projectedTriangles = shadowProj viewProjMat <$> triangles

        frags :: FragmentStream () <- rasterize shadowRasterOptions projectedTriangles
        let
            shadowFragsWithDepth = flip withRasterizedInfo frags $ \_ p ->
                (undefined, let (z, w) = (rasterizedFragCoord p ^. _z, rasterizedFragCoord p ^. _w) in z/w)
            depthOption = DepthOption Less True

            -- doDrawColor a = drawColor (\env -> (colorImage env, pure True, False)) a
            doDrawColor _ = return ()

        drawDepth (\env -> (NoBlending, shadowImage env, depthOption)) shadowFragsWithDepth doDrawColor

    shader <- compileShader $ do
        config <- getUniform (const (shaderConfigUniformBuffer, 0))
        let viewProjMat = shaderConfigProjectionS config !*! shaderConfigCameraS config !*! shaderConfigTransformationS config
            camPos = shaderConfigCameraPositionS config
            normMat = m44to33 (shaderConfigTransformationS config)
            shadowMat = shaderConfigShadowS config
            lightingContext =
                ( camPos
                , shaderConfigFogS config
                , shaderConfigSunS config
                , 0.8 -- material specular intensity
                , 8 -- material specular power
                )

        triangles :: PrimitiveStream Triangles (V3 VFloat, V3 VFloat) <- toPrimitiveStream primitives
        let
            projectedTriangles :: PrimitiveStream Triangles (V4 VFloat, (V3 VFloat, V4 VFloat, V3 VFloat, LightingContext V))
            projectedTriangles = projWithShadow viewProjMat normMat shadowMat lightingContext <$> triangles

        let samplerFilter = SamplerFilter Nearest Nearest Nearest Nothing
            edge = (pure ClampToEdge, 0)
        sampler <- newSampler2D (const (fst shadowTex, samplerFilter, edge))

        fragNormals :: FragmentStream (V3 FFloat, V4 FFloat, V3 FFloat, LightingContext F) <- rasterize rasterOptions projectedTriangles
        let
            sampleTexture = sample2D sampler SampleAuto Nothing Nothing
            -- litFrags = filterFragments ((>* 0.5) . sampleTexture) fragNormals
            litFrags = simpleLightWithShadow sampleTexture <$> fragNormals
            litFragsWithDepth = withRasterizedInfo
                (\a p -> (a, rasterizedFragCoord p ^. _z)) litFrags
            colorOption = ContextColorOption NoBlending (pure True)
            depthOption = DepthOption Less True

        drawWindowColorDepth (const (window, colorOption, depthOption)) litFragsWithDepth

    gridShader <- compileShader $ do
        config <- getUniform (const (shaderConfigUniformBuffer, 0))
        let viewProjMat = shaderConfigProjectionS config !*! shaderConfigCameraS config !*! shaderConfigTransformationS config
            camPos = shaderConfigCameraPositionS config

        triangles :: PrimitiveStream Triangles (V3 VFloat) <- toPrimitiveStream gridPrimitives
        let
            projectedTriangles :: PrimitiveStream Triangles (V4 VFloat, (V2 VFloat, VFloat, FogS V))
            projectedTriangles =
                (\p -> (viewProjMat !* p, (v4To2 p, camPos ^._z, shaderConfigFogS config))) .
                v3To4 .
                (* 4000) <$> triangles

        fragCoord :: FragmentStream (V2 FFloat, FFloat, FogS F) <- rasterize gridRasterOptions projectedTriangles
        let
            drawGridLine :: (V2 FFloat, FFloat, FogS F) -> V4 FFloat
            drawGridLine (p, camPosZ, fog) = color' where
                -- Pick a coordinate to visualize in a grid
                coord = p / 40
                -- Compute anti-aliased world-space grid lines
                V2 gx gy = abs (fract' (coord - 0.5) - 0.5) / (fwidth <$> coord)
                line = minB gx gy
                -- Just visualize the grid lines directly
                color = V4 1 0 0 (1 - minB (line) 1)
                -- Add fog.
                fogDistance = norm $ V3 (p^._x) (p^._y) camPosZ
                color' = applyFog fog color (fogDistance * 0.01)

            litFrags = drawGridLine <$> fragCoord

            litFragsWithDepth = withRasterizedInfo
                (\a p -> (a, rasterizedFragCoord p ^._z)) litFrags
            blending = BlendRgbAlpha
                (FuncAdd, FuncAdd)
                (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors Zero Zero)
                (pure 1)
            colorOption = ContextColorOption blending (pure True)
            depthOption = DepthOption Less True

        drawWindowColorDepth (const (window, colorOption, depthOption)) litFragsWithDepth

    let
        renderObjects viewPort ShadowMappingStage = do
            sImage <- getTexture2DImage (fst shadowTex) 0
            clearImageDepth sImage 1
            primArray <- mconcat <$> makePrimArrays
            shadowShader $ ShadowShaderEnvironment primArray sImage (Front, viewPort, DepthRange 0 1)
        renderObjects viewPort DirectShadingStage = do
            clearWindowDepth window 1 -- Far plane
            primArray <- mconcat <$> makePrimArrays
            shader $ ShaderEnvironment primArray (Front, viewPort, DepthRange 0 1)
        renderObjects _ _ = return ()

        renderGrid viewPort DirectShadingStage = do
            gridPrimArray <- makeGridPrimArray
            gridShader $ GridShaderEnvironment gridPrimArray (FrontAndBack, viewPort, DepthRange 0 1)
        renderGrid _ _ = return ()

    let
        sun = (DirectionLight (V3 1.0 0.8 0.3) (- signorm (V3 20 5 15)) 0.1)
        world = World
            shaderConfigUniformBuffer
            pointLightUniformBuffer
            [renderObjects, renderGrid]
            []
            sun
            -- [("first-camera", Camera (V3 20 5 15) (pi/3) (-pi) (pi/3))]
            [("first-camera", Camera (V3 10 10 10) (pi/4) (pi*5/4) (pi/3))]
            0
            shadowTex
            colorTex
    return world

-- Project a (vertice, normal)
proj :: Floating a
    => V4 (V4 a)
    -> V3 (V3 a)
    -> DirectionLightS V
    -> (V3 a, V3 a)
    -> (V4 a, (V3 a, DirectionLightS V))
proj modelViewProj normMat sun (p, normal) =
    (modelViewProj !* v3To4 p, (normMat !* normal, sun))

type LightingContext x =
    ( V3 (S x Float) -- camPos
    , FogS x
    , DirectionLightS x -- sun
    , S x Float -- material specular intensity
    , S x Float -- material specular power
    )

projWithShadow :: Floating a
    => V4 (V4 a)
    -> V3 (V3 a)
    -> V4 (V4 a)
    -> LightingContext V
    -> (V3 a, V3 a)
    -> (V4 a, (V3 a, V4 a, V3 a, LightingContext V))
projWithShadow modelViewProj normMat shadowMat lightingContext (position, normal) =
    let p = v3To4 position
    -- TODO position is not transformed
    in  (modelViewProj !* p, (normMat !* normal, shadowMat !* p, position, lightingContext))

simpleLightWithShadow :: (V2 FFloat -> ColorSample F Depth)
    -> (V3 FFloat, V4 FFloat, V3 FFloat, LightingContext F)
    -> V4 FFloat
simpleLightWithShadow shadowMap (normal, shadowCoord, worldSpacePosition, lightingContext) =
    let (cameraPosition, fog, sun, specularIntensity, specularPower) = lightingContext
        DirectionLightS sunLightColor sunLightDirection sunLightAmbientIntensity = sun
        cameraDirection = signorm (cameraPosition - worldSpacePosition)
        baseColor = pure 1 -- V4 0.4 0.4 0.4 1

        -- Sun
        ambient = sunLightColor ^* sunLightAmbientIntensity
        diffuse = sunLightColor ^* maxB 0 (dot normal (-sunLightDirection))
        specular = getSpecularColor cameraDirection normal specularIntensity specularPower (sunLightColor * 1.5) (-sunLightDirection)
        shadow = getShadow shadowMap normal (-sunLightDirection) shadowCoord
        sunContribution = ambient + (diffuse + specular) ^* shadow

        -- Lights
        lightContributions = pure 0
        {-
        lights :: [PointLight] = []
        lightContributions = sum $ flip map lights $ \(lightPosition, lightColor) ->
            lightDirection = normalize (lightPosition - worldSpacePosition)
            lightDirection = length (lightPosition - worldSpacePosition)
            attenuation = 0.01 + 0.07 * lightDistance + 0.00008 * lightDistance * lightDistance

            diffuse = lightColor * maxB 0 (dot normal lightDirection)
            specular = getSpecularColor cameraDirection normal specularIntensity specularPower lightColor lightDirection
            baseColor * (diffuse + specular) / attenuation
        -}

    in  baseColor * (v3To4 sunContribution + lightContributions)

getFogFactor :: FogS F -> FFloat -> FFloat
getFogFactor fog fogDistance = 1 - clamp factor 0 1 where
    -- factor = (fogEnd - fogDistance) / (fogEnd - fogStart)
    factor = exp (-(fogDensityS fog) * fogDistance)
    -- factor = exp (-pow (fogDensity * fogDistance, 2.0))
    -- factor = 0.0

applyFog :: FogS F -> V4 FFloat -> FFloat -> V4 FFloat
applyFog fog color fogDistance = mix color (fogColorS fog) (V4 a a a 0) where a = getFogFactor fog fogDistance

getSpecularColor :: V3 FFloat
    -> V3 FFloat
    -> FFloat
    -> FFloat
    -> V3 FFloat
    -> V3 FFloat
    -> V3 FFloat
getSpecularColor cameraDirection normal specularIntensity specularPower lightColor rayDirection =
    let bouncingRayDirection = signorm (reflect (-rayDirection) normal)
        specularFactor = dot cameraDirection bouncingRayDirection

    in ifThenElse' (specularFactor >* 0)
        (lightColor ^* (specularIntensity * (specularFactor ** specularPower)))
        (pure 0)

{- | Calculate the reflection direction for an incident vector.
For a given incident vector I and surface normal N reflect returns the
reflection direction calculated as I - 2.0 * dot(N, I) * N. N should be
normalized in order to achieve the desired result. 
-}
reflect :: V3 FFloat -- ^ Specifies the incident vector. 
    -> V3 FFloat -- ^ Specifies the normal vector. 
    -> V3 FFloat
reflect i n = i - 2 ^* dot n i * n

getShadow :: (V2 FFloat -> ColorSample F Depth) -> V3 FFloat ->  V3 FFloat -> V4 FFloat -> FFloat
getShadow shadowMap normal sunLightDirection (V4 x y z w) = maxB 0 visibility where
    rectify c = (c/w + 1) / 2
    texCoords = V2 (rectify x) (rectify y)

    cosTheta = clamp (dot normal sunLightDirection) 0 1
    bias = clamp (0.005 * tan (acos cosTheta)) 0 0.01

    sample offset = shadowMap (texCoords + offset / 700)
    getContribution zShadow = ifThenElse' (zShadow <* rectify z - bias) 1 0

    visibility = 1 - 0.75 * sum ((getContribution . sample) <$> poissonDisk) / fromIntegral (length poissonDisk)

poissonDisk :: [V2 FFloat]
poissonDisk =
   [ V2 (-0.94201624) (-0.39906216)
   , V2 (0.94558609) (-0.76890725)
   , V2 (-0.094184101) (-0.92938870)
   , V2 (0.34495938) (0.29387760)
   , V2 (-0.91588581) (0.45771432)
   , V2 (-0.81544232) (-0.87912464)
   , V2 (-0.38277543) (0.27676845)
   , V2 (0.97484398) (0.75648379)
   , V2 (0.44323325) (-0.97511554)
   , V2 (0.53742981) (-0.47373420)
   , V2 (-0.26496911) (-0.41893023)
   , V2 (0.79197514) (0.19090188)
   , V2 (-0.24188840) (0.99706507)
   , V2 (-0.81409955) (0.91437590)
   , V2 (0.19984126) (0.78641367)
   , V2 (0.14383161) (-0.14100790)
   ]

v3To4 :: Floating a => V3 a -> V4 a
v3To4 (V3 x y z) = V4 x y z 1

v4To3 :: Floating a => V4 a -> V3 a
v4To3 (V4 x y z w) = V3 x y z

v4To3' :: Floating a => V4 a -> V3 a
v4To3' (V4 x y z w) = V3 (x/w) (y/w) (z/w)

v4To2 :: Floating a => V4 a -> V2 a
v4To2 (V4 x y _ _) = V2 x y

v4To2' :: Floating a => V4 a -> V2 a
v4To2' (V4 x y _ w) = V2 (x/w) (y/w)

m44to33 :: Floating a => M44 a -> M33 a
m44to33 = v4To3 . fmap v4To3

shadowProj :: Floating a
    => V4 (V4 a)
    -> (V3 a, V3 a)
    -> (V4 a, ())
shadowProj modelViewProj (p, _) = (modelViewProj !* v3To4 p, ())

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
