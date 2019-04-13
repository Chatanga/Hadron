{-# LANGUAGE Arrows, FlexibleInstances, UndecidableInstances #-}
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
import Control.Arrow
import "lens" Control.Lens

import Linear
import Graphics.GPipe
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW

import Graphics.Geometry

------------------------------------------------------------------------------------------------------------------------

data Stage
    = ShadowMappingStage
    | DirectShadingStage
    | DeferredShadingStage
    deriving (Eq)

class Renderable a where
    toRenderables :: a -> ContextT GLFW.Handle os IO [ViewPort -> Render os ()]

data FireBall = FireBall
    { fireBallPosition :: !(V3 Float)
    , fireBallDirection :: !(V3 Float)
    , fireBallColor :: !(V3 Float)
    , fireBallAge :: !Double
    } deriving (Show)

instance Renderable FireBall where
    toRenderables (FireBall p d (V3 cx cy cz) a) = undefined

data DirectionLight = DirectionLight
    { directionLightColor :: !(V3 Float)
    , directionLightDirection :: !(V3 Float)
    , directionLightAmbientIntensity :: !Float
    } deriving (Show)

data DirectionLightB = DirectionLightB
    { directionLightColorB :: !(B3 Float)
    , directionLightDirectionB :: !(B3 Float)
    , directionLightAmbientIntensityB :: !(B Float)
    }

instance BufferFormat DirectionLightB where
    type HostFormat DirectionLightB = DirectionLight
    toBuffer = proc ~(DirectionLight c d i) -> do
            (c', d', i') <- toBuffer -< (c, d, i)
            returnA -< (DirectionLightB c' d' i')

data DirectionLightS x = DirectionLightS
    { directionLightColorS :: !(V3 (S x Float))
    , directionLightDirectionS :: !(V3 (S x Float))
    , directionLightAmbientIntensityS :: !(S x Float)
    }

instance UniformInput DirectionLightB where
    type UniformFormat DirectionLightB x = DirectionLightS x
    toUniform = proc ~(DirectionLightB c d i) -> do
            (c', d', i') <- toUniform -< (c, d, i)
            returnA -< (DirectionLightS c' d' i')

instance VertexInput DirectionLight where
    type VertexFormat DirectionLight = DirectionLightS V
    toVertex = proc ~(DirectionLight c d i) -> do
            (c', d', i') <- toVertex -< (c, d, i)
            returnA -< (DirectionLightS c' d' i')

instance FragmentInput (DirectionLightS V) where
    type FragmentFormat (DirectionLightS V) = DirectionLightS F
    toFragment = proc ~(DirectionLightS c d i) -> do
            (c', d', i') <- toFragment -< (c, d, i)
            returnA -< (DirectionLightS c' d' i')

data PointLight = PointLight
    { pointLightPosition :: !(V3 Float)
    , pointLightColor :: !(V3 Float)
    } deriving (Show)

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

type WorldUniform os = Buffer os (Uniform (V4 (B4 Float), V3 (B3 Float), DirectionLightB, (V4 (B4 Float))))

data World os = World
    { worldUniform :: WorldUniform os
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

    uniform :: WorldUniform os <- newBuffer 1

    gridBuffer :: Buffer os (B3 Float) <- newBuffer 6
    writeBuffer gridBuffer 0
        [ V3 1 1 0,  V3 (-1) 1 0,  V3 1 (-1) 0
        , V3 (-1) (-1) 0,  V3 1 (-1) 0,  V3 (-1) 1 0
        ]

    wallBuffer :: Buffer os (B3 Float, B3 Float) <- newBuffer (length walls)
    writeBuffer wallBuffer 0 walls

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
            return [a, b]

    shadowTex <- newTexture2D' Depth32F (V2 800 600) 1
    colorTex <- newTexture2D' RGB8 (V2 800 600) 1

    shadowShader <- compileShader $ do
        (viewProjMat, _, _, _) <- getUniform (const (uniform, 0))

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
        (viewProjMat :: V4 (V4 (S V Float)), normMat, sun :: DirectionLightS V, shadowMat) <- getUniform (const (uniform, 0))

        triangles :: PrimitiveStream Triangles (V3 VFloat, V3 VFloat) <- toPrimitiveStream primitives
        let
            projectedTriangles :: PrimitiveStream Triangles (V4 VFloat, (V3 VFloat, DirectionLightS V, V4 VFloat))
            projectedTriangles = projWithShadow viewProjMat normMat sun shadowMat <$> triangles

        let samplerFilter = SamplerFilter Nearest Nearest Nearest Nothing
            edge = (pure ClampToEdge, 0)
        sampler <- newSampler2D (const (fst shadowTex, samplerFilter, edge))

        fragNormals :: FragmentStream (V3 (S F Float), DirectionLightS F, V4 (S F Float)) <- rasterize rasterOptions projectedTriangles
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
        (viewProjMat, _, _, _) <- getUniform (const (uniform, 0))

        triangles :: PrimitiveStream Triangles (V3 VFloat) <- toPrimitiveStream gridPrimitives
        let
            camPos = viewProjMat ^.column _w
            projectedTriangles :: PrimitiveStream Triangles (V4 VFloat, (V2 VFloat, VFloat))
            projectedTriangles =
                (\p -> (viewProjMat !* p, (v4To2 p, camPos^._z))) .
                v3To4 .
                (* 4000) <$> triangles

        fragCoord :: FragmentStream (V2 FFloat, FFloat) <- rasterize gridRasterOptions projectedTriangles
        let
            fogColor = V4 0.5 0.5 0.5 1
            fogDensity = 0.2
            (fogStart, fogEnd) = (10, 100)

            getFogFactor :: S F Float -> S F Float
            getFogFactor fogDistance = 1.0 - clamp factor 0.0 1.0 where
                -- factor = (fogEnd - fogDistance) / (fogEnd - fogStart)
                factor = exp (-fogDensity * fogDistance)
                -- factor = exp (-pow (fogDensity * fogDistance, 2.0))
                -- factor = 0.0

            applyFog :: V4 (S F Float) -> S F Float -> V4 (S F Float)
            applyFog color fogDistance = mix color fogColor (V4 a a a 0) where a = getFogFactor fogDistance

            drawGridLine :: (V2 FFloat, FFloat) -> V4 (S F Float)
            drawGridLine (p, camPosZ) = color' where
                -- Pick a coordinate to visualize in a grid
                coord = p / 40
                -- Compute anti-aliased world-space grid lines
                V2 gx gy = abs (fract' (coord - 0.5) - 0.5) / (fwidth <$> coord)
                line = minB gx gy
                -- Just visualize the grid lines directly
                color = V4 1 0 0 (1 - minB (line) 1)
                -- Add fog.
                fogDistance = norm $ V3 (p^._x) (p^._y) camPosZ
                color' = applyFog color (fogDistance * 0.01)

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
        renderIncal viewPort ShadowMappingStage = do
            sImage <- getTexture2DImage (fst shadowTex) 0
            clearImageDepth sImage 1
            primArray <- mconcat <$> makePrimArrays
            shadowShader $ ShadowShaderEnvironment primArray sImage (FrontAndBack, viewPort, DepthRange 0 1)
        renderIncal viewPort DirectShadingStage = do
            clearWindowDepth window 1 -- Far plane
            primArray <- mconcat <$> makePrimArrays
            shader $ ShaderEnvironment primArray (FrontAndBack, viewPort, DepthRange 0 1)
        renderIncal _ _ = return ()

        renderGrid viewPort DirectShadingStage = do
            gridPrimArray <- makeGridPrimArray
            gridShader $ GridShaderEnvironment gridPrimArray (FrontAndBack, viewPort, DepthRange 0 1)
        renderGrid _ _ = return ()

    let
        -- sun = (DirectionLight (V3 0.6 0.8 1.0) (signorm (V3 0.5 0.75 1.0)) 1.0)
        sun = (DirectionLight (V3 0.6 0.8 1.0) (- signorm (V3 20 5 15)) 1.0)
        world = World
            uniform
            [renderIncal, renderGrid]
            []
            sun
            [("first-camera", Camera (V3 20 5 15) (pi/3) (-pi) (pi/3))]
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

projWithShadow :: Floating a
    => V4 (V4 a)
    -> V3 (V3 a)
    -> DirectionLightS V
    -> V4 (V4 a)
    -> (V3 a, V3 a)
    -> (V4 a, (V3 a, DirectionLightS V, V4 a))
projWithShadow modelViewProj normMat sun shadowMat (position, normal) =
    let p = v3To4 position
    in  (modelViewProj !* p, (normMat !* normal, sun, shadowMat !* p))

simpleLightWithShadow :: (V2 (S F Float) -> ColorSample F Depth)
    -> (V3 (S F Float), DirectionLightS F, V4 (S F Float))
    -> V4 (S F Float)
simpleLightWithShadow shadowMap (normal, DirectionLightS color direction _, shadowCoord) =
    let litColor = color * pure (normal `dot` (-direction))
        shadow = getShadow shadowMap normal direction shadowCoord
    in  v3To4 (litColor ^* shadow)

{-
    // Shadow.
    float visibility;
    if (shadowUsed == TRUE) {
        vec4 shadowCoord = shadow * theWorldSpacePosition;
        visibility = getVisibility3(normal, shadowCoord);
    } else {
        visibility = 1.0;
    }

    // Sunlight.
    vec3 ambient = sunLightColor * sunLightAmbientIntensity;
    vec3 diffuse = sunLightColor * max(0.0, dot(normal, normalize(-sunLightDirection)));
    vec3 specular = getSpecularColor(theWorldSpacePosition, normal, specularIntensity, specularPower, sunLightColor, sunLightDirection);
    vec4 fragColor = baseColor * vec4(ambient + (diffuse + specular) * visibility, 1.0);

    // Light contributions.
    for (int i = 0; i < min(10, lightCount); ++i) {
        vec3 lightDirection = lightPositions[i] - theWorldSpacePosition.xyz;
        float lightDistance = length(lightDirection);
        float attenuation = 0.01 + 0.07 * lightDistance + 0.00008 * lightDistance * lightDistance;

        vec3 diffuse = lightColors[i] * max(0.0, dot(normal, normalize(lightDirection)));
        vec3 specular = getSpecularColor(theWorldSpacePosition, normal, specularIntensity, specularPower, lightColors[i], lightDirection);
        fragColor += baseColor * vec4((diffuse + specular) / attenuation, 0.0);
    }

    // Add fog.
    float fogDistance = length(theCameraSpacePosition);
    fragColor = applyFog(fragColor, fogDistance);

    return fragColor;
-}

getSpecularColor :: V3 (S F Float)
    -> V4 (S F Float)
    -> V3 (S F Float)
    -> S F Float
    -> S F Float
    -> V3 (S F Float)
    -> V3 (S F Float)
    -> V3 (S F Float)
getSpecularColor cameraPosition worldSpacePosition normal specularIntensity specularPower lightColor rayDirection =
    let bouncingRayDirection :: V3 (S F Float) = signorm (reflect rayDirection normal)
        cameraDirection :: V3 (S F Float) = signorm (cameraPosition - v4To3 worldSpacePosition)
        specularFactor :: S F Float = dot cameraDirection bouncingRayDirection

    in ifThenElse' (specularFactor >* 0)
        (lightColor ^* (specularIntensity * (specularFactor ** specularPower)))
        (pure 0)

{- | Calculate the reflection direction for an incident vector.
For a given incident vector I and surface normal N reflect returns the
reflection direction calculated as I - 2.0 * dot(N, I) * N. N should be
normalized in order to achieve the desired result. 
-}
reflect :: V3 (S F Float) -- ^ Specifies the incident vector. 
    -> V3 (S F Float) -- ^ Specifies the normal vector. 
    -> V3 (S F Float)
reflect i n = i - 2 ^* dot n i * n

getShadow :: (V2 (S F Float) -> ColorSample F Depth) -> V3 (S F Float) ->  V3 (S F Float) -> V4 (S F Float) -> S F Float
getShadow shadowMap normal sunLightDirection (V4 x y z w) = maxB 0 visibility where
    rectify c = (c/w + 1) / 2
    texCoords = V2 (rectify x) (rectify y)

    cosTheta = clamp (dot normal sunLightDirection) 0 1
    bias = clamp (0.0005 * tan (acos cosTheta)) 0 0.001

    sample offset = shadowMap (texCoords + offset / 700)
    getContribution zShadow = ifThenElse' (zShadow <* rectify z - bias) 1 0

    visibility = 1 - 0.75 * sum ((getContribution . sample) <$> poissonDisk) / fromIntegral (length poissonDisk)

poissonDisk :: [V2 (S F Float)]
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
v4To3 (V4 x y z _) = V3 x y z

v4To2 :: Floating a => V4 a -> V2 a
v4To2 (V4 x y _ _) = V2 x y

shadowProj :: Floating a
    => V4 (V4 a)
    -> (V3 a, V3 a)
    -> (V4 a, ())
shadowProj modelViewProj (p, _) = (modelViewProj !* v3To4 p, ())

walls :: [(V3 Float, V3 Float)]
walls = zip vertices (extractNormals vertices) where
    (p0, p1, p2, p3, p4, p5, p6) = (V3 0 0 0, V3 1 0 0, V3 0 1 0, V3 0 0 1, V3 1 1 0, V3 0 1 1, V3 1 0 1)
    vertices = map (\x -> x - V3 10 10 10) $ map (^* 20) $
        [ p0, p2, p1
        , p1, p2, p4
        , p0, p3, p2
        , p2, p3, p5
        , p0, p1, p3
        , p1, p6, p3
        ]

incal :: [(V3 Float, V3 Float)]
incal = zip vertices (extractNormals vertices) where
    (pO, pA, pB, pC, pD) = (V3 0 0 2, V3 2 2 (-1), V3 (-2) 2 (-1), V3 (-2) (-2) (-1), V3 2 (-2) (-1))
    pyramid =
        [ pA, pB, pD
        , pC, pD, pB
        , pA, pO, pB
        , pB, pO, pC
        , pC, pO, pD
        , pD, pO, pA
        ]
    vertices = map (^* 2) $ pyramid ++ map (rotate (axisAngle (V3 1 0 0) pi)) pyramid

calculateTriangleNormal :: Floating a => V3 a -> V3 a -> V3 a -> V3 a
calculateTriangleNormal p1 p2 p3 = signorm $ cross (p3 ^-^ p1) (p2 ^-^ p1)

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
