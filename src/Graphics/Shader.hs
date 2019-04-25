{-# LANGUAGE Arrows, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Graphics.Shader
    ( DirectionLight(..)
    , PointLight(..)
    , FogEquation(..)
    , Fog(..)
    , ShaderConfig(..)
    , FrameBufferGroup(..)
    , RenderContext(..)
    , createRenderer
    ) where

import Prelude hiding ((<*))

import Control.Applicative (pure)
import Control.Arrow
import Control.Monad
import Control.Monad.State
import "lens" Control.Lens

import Data.Int

import Linear
import Graphics.GPipe
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW

import Graphics.Geometry
import Graphics.Texture
import Common.Random

------------------------------------------------------------------------------------------------------------------------

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

------------------------------------------------------------------------------------------------------------------------

data PointLight = PointLight
    { pointLightPosition :: !(V3 Float)
    , pointLightColor :: !(V3 Float)
    } deriving (Show)

data PointLightB = PointLightB
    { pointLightPositionB :: !(B3 Float)
    , pointLightColorB :: !(B3 Float)
    }

instance BufferFormat PointLightB where
    type HostFormat PointLightB = PointLight
    toBuffer = proc ~(PointLight p c) -> do
            (p', c') <- toBuffer -< (p, c)
            returnA -< (PointLightB p' c')

data PointLightS x = PointLightS
    { pointLightPositionS :: !(V3 (S x Float))
    , pointLightColorS :: !(V3 (S x Float))
    }

instance UniformInput PointLightB where
    type UniformFormat PointLightB x = PointLightS x
    toUniform = proc ~(PointLightB p c) -> do
            (p', c') <- toUniform -< (p, c)
            returnA -< (PointLightS p' c')

instance VertexInput PointLight where
    type VertexFormat PointLight = PointLightS V
    toVertex = proc ~(PointLight p c) -> do
            (p', c') <- toVertex -< (p, c)
            returnA -< (PointLightS p' c')

instance FragmentInput (PointLightS V) where
    type FragmentFormat (PointLightS V) = PointLightS F
    toFragment = proc ~(PointLightS p c) -> do
            (p', c') <- toFragment -< (p, c)
            returnA -< (PointLightS p' c')

------------------------------------------------------------------------------------------------------------------------

data FogEquation = Linear | Exp | Exp2
    deriving (Show)

data Fog = Fog
    { fogColor :: !(V4 Float)
    , fogStart :: !Float
    , fogEnd :: !Float
    , fogDensity :: !Float
    -- , fogEquation :: !FogEquation
    } deriving (Show)

data FogB = FogB
    { fogColorB :: !(B4 Float)
    , fogStartB :: !(B Float)
    , fogEndB :: !(B Float)
    , fogDensityB :: !(B Float)
    }

instance BufferFormat FogB where
    type HostFormat FogB = Fog
    toBuffer = proc ~(Fog c s e d) -> do
            (c', s', e', d') <- toBuffer -< (c, s, e, d)
            returnA -< (FogB c' s' e' d')

data FogS x = FogS
    { fogColorS :: !(V4 (S x Float))
    , fogStartS :: !(S x Float)
    , fogEndS :: !(S x Float)
    , fogDensityS :: !(S x Float)
    }

instance UniformInput FogB where
    type UniformFormat FogB x = FogS x
    toUniform = proc ~(FogB c s e d) -> do
            (c', s', e', d') <- toUniform -< (c, s, e, d)
            returnA -< (FogS c' s' e' d')

instance VertexInput Fog where
    type VertexFormat Fog = FogS V
    toVertex = proc ~(Fog c s e d) -> do
            (c', s', e', d') <- toVertex -< (c, s, e, d)
            returnA -< (FogS c' s' e' d')

instance FragmentInput (FogS V) where
    type FragmentFormat (FogS V) = FogS F
    toFragment = proc ~(FogS c s e d) -> do
            (c', s', e', d') <- toFragment -< (c, s, e, d)
            returnA -< (FogS c' s' e' d')

------------------------------------------------------------------------------------------------------------------------

data ShaderConfig = ShaderConfig
    { shaderConfigCameraPosition :: !(V3 Float)
    
    , shaderConfigProjection :: !(M44 Float)
    , shaderConfigCamera :: !(M44 Float)
    , shaderConfigTransformation :: !(M44 Float)

    -- , shaderConfigViewProjMat :: !(M44 Float) <-- calculated in shader
    -- , shaderConfigNormalMat :: !(V3 Float) <-- calculated in shader

    , shaderConfigShadowUsed :: !Int32
    , shaderConfigShadow :: !(M44 Float)

    , shaderConfigFog :: Fog
    , shaderConfigSun :: DirectionLight

    , shaderConfigTimePassed :: !Float
    }

data ShaderConfigB = ShaderConfigB
    { shaderConfigCameraPositionB :: !(B3 Float)
    
    , shaderConfigProjectionB :: !(V4 (B4 Float))
    , shaderConfigCameraB :: !(V4 (B4 Float))
    , shaderConfigTransformationB :: !(V4 (B4 Float))

    , shaderConfigShadowUsedB :: !(B Int32)
    , shaderConfigShadowB :: !(V4 (B4 Float))

    , shaderConfigFogB :: FogB
    , shaderConfigSunB :: DirectionLightB

    , shaderConfigTimePassedB :: !(B Float)
    }

instance BufferFormat ShaderConfigB where
    type HostFormat ShaderConfigB = ShaderConfig
    toBuffer = proc ~(ShaderConfig a b c d e f g h i) -> do
            ((a', b', c', d', e', f'), (g', h', i')) <- toBuffer -< ((a, b, c, d, e, f), (g, h, i))
            returnA -< (ShaderConfigB a' b' c' d' e' f' g' h' i')

data ShaderConfigS x = ShaderConfigS
    { shaderConfigCameraPositionS :: !(V3 (S x Float))
    
    , shaderConfigProjectionS :: !(M44 (S x Float))
    , shaderConfigCameraS :: !(M44 (S x Float))
    , shaderConfigTransformationS :: !(M44 (S x Float))

    , shaderConfigShadowUsedS :: !(S x Int)
    , shaderConfigShadowS :: !(M44 (S x Float))

    , shaderConfigFogS :: FogS x
    , shaderConfigSunS :: DirectionLightS x

    , shaderConfigTimePassedS :: !(S x Float)
    }

instance UniformInput ShaderConfigB where
    type UniformFormat ShaderConfigB x = ShaderConfigS x
    toUniform = proc ~(ShaderConfigB a b c d e f g h i) -> do
            ((a', b', c', d', e', f'), (g', h', i')) <- toUniform -< ((a, b, c, d, e, f), (g, h, i))
            returnA -< (ShaderConfigS a' b' c' d' e' f' g' h' i')

instance VertexInput ShaderConfig where
    type VertexFormat ShaderConfig = ShaderConfigS V
    toVertex = proc ~(ShaderConfig a b c d e f g h i) -> do
            ((a', b', c', d', e', f'), (g', h', i')) <- toVertex -< ((a, b, c, d, e, f), (g, h, i))
            returnA -< (ShaderConfigS a' b' c' d' e' f' g' h' i')

instance FragmentInput (ShaderConfigS V) where
    type FragmentFormat (ShaderConfigS V) = ShaderConfigS F
    toFragment = proc ~(ShaderConfigS a b c d e f g h i) -> do
            ((a', b', c', d', e', f'), (g', h', i')) <- toFragment -< ((a, b, c, d, e, f), (g, h, i))
            returnA -< (ShaderConfigS a' b' c' d' e' f' g' h' i')

------------------------------------------------------------------------------------------------------------------------

data FrameBufferGroup os = FrameBufferGroup
    { frameBufferGroupShadowTex :: Texture2D os (Format Depth)
    , frameBufferGroupDepthTex :: Texture2D os (Format Depth)
    , frameBufferGroupPositionTex :: Texture2D os (Format RGBFloat)
    , frameBufferGroupNormalTex :: Texture2D os (Format RGBFloat)
    , frameBufferGroupMaterialTex :: Texture2D os (Format RGBAFloat)
    , frameBufferGroupOcclusionTex :: Texture2D os (Format Depth)
    , frameBufferGroupBlurredOcclusionTex :: Texture2D os (Format Depth)
    {-
    , frameBufferGroupHdrColorTex :: Texture2D os (Format RGBFloat)
    , frameBufferGroupLdrColorTex :: Texture2D os (Format RGBFloat)
    , frameBufferGroupBloomColorTex :: Texture2D os (Format RGBFloat)
    -}        
    }

data RenderContext os = RenderContext
    { renderContextFrameBufferGroup :: Maybe (Size2, FrameBufferGroup os)
    , renderContextRenderAction
        :: RenderContext os
        -> ((Int, Int), (Int, Int))
        -> Camera
        -> DirectionLight
        -> [PointLight]
        -> [Buffer os (B3 Float, B3 Float)]
        -> ContextT GLFW.Handle os IO (RenderContext os)
    }

createFrameBufferGroup :: Size2 -> ContextT GLFW.Handle os IO (Size2, FrameBufferGroup os)
createFrameBufferGroup size = do
    frameBufferGroup <- FrameBufferGroup
        <$> newTexture2D Depth16 (V2 2048 2048) 1
        <*> newTexture2D Depth16 size 1
        <*> newTexture2D RGB16F size 1
        <*> newTexture2D RGB16F size 1
        <*> newTexture2D RGBA8 size 1
        <*> newTexture2D Depth16 size 1
        <*> newTexture2D Depth16 size 1
    return (size, frameBufferGroup)

createRenderer :: Window os RGBAFloat Depth -> ContextT GLFW.Handle os IO (RenderContext os)
createRenderer window = do

    shaderConfigUniformBuffer :: Buffer os (Uniform ShaderConfigB) <- newBuffer 1
    pointLightUniformBuffer :: Buffer os (Uniform PointLightB) <- newBuffer 8

    let screen =
            [ V2 1 1,  V2 (-1) 1,  V2 1 (-1)
            , V2 (-1) (-1),  V2 1 (-1),  V2 (-1) 1
            ]
    screenBuffer :: Buffer os (B2 Float) <- newBuffer (length screen)
    writeBuffer screenBuffer 0 screen

    let grid =
            [ V3 1 1 0,  V3 (-1) 1 0,  V3 1 (-1) 0
            , V3 (-1) (-1) 0,  V3 1 (-1) 0,  V3 (-1) 1 0
            ]
    gridBuffer :: Buffer os (B3 Float) <- newBuffer (length grid)
    writeBuffer gridBuffer 0 grid

    sampleKernelTex <- generateSampleKernelTexture 64

    noiseTex <- generateNoiseTexture (4, 4)

    compiledShadowShader <- compileShader $ shadowShader window shaderConfigUniformBuffer
    compiledDirectShader <- compileShader . silenceShader $ directShader window shaderConfigUniformBuffer
    compiledDeferredShader <- compileShader $ deferredShader window shaderConfigUniformBuffer
    compiledSsaoShader <- compileShader $ ssaoShader window shaderConfigUniformBuffer
    compiledBlurShader <- compileShader $ blurShader window
    compiledScreenShader <- compileShader . silenceShader $ screenShader window
    compiledLightingShader <- compileShader $ lightingShader window shaderConfigUniformBuffer
    compiledGridShader <- compileShader $ gridShader window shaderConfigUniformBuffer

    let renderAction context bounds camera sun lights buffers = do
            let (_, (w, h)) = bounds
                size = V2 w h
            frameBufferGroup <- case renderContextFrameBufferGroup context of
                Just frameBufferGroup -> if fst frameBufferGroup == size
                    then return frameBufferGroup
                    else createFrameBufferGroup size
                Nothing -> createFrameBufferGroup size

            shadowMat <- renderShadow bounds (snd frameBufferGroup) camera sun buffers
            renderObjects shadowMat bounds (snd frameBufferGroup) camera sun lights buffers

            return $ RenderContext (Just frameBufferGroup) renderAction

        renderShadow bounds frameBufferGroup camera sun buffers = do
            let
                r = far / 50
                projectionMat = ortho (-r) r (-r) r (-r) r
                localPosition = V3 0 0 0
                cameraMat = lookAt'
                    localPosition
                    (directionLightDirection sun)
                    (getUp camera)
                biasMat = V4
                    (V4 0.5 0.0 0.0 0.5)
                    (V4 0.0 0.5 0.0 0.5)
                    (V4 0.0 0.0 0.5 0.5)
                    (V4 0.0 0.0 0.0 1.0)
                -- shadowMat = biasMat !*! projectionMat !*! cameraMat
                shadowMat = projectionMat !*! cameraMat

            writeBuffer shaderConfigUniformBuffer 0 [ShaderConfig
                (cameraPosition camera)
                projectionMat
                cameraMat
                identity -- Transformation
                1 -- ShadowUsed
                shadowMat
                (Fog (V4 0.5 0.5 0.5 1) 10 100 0.2)
                sun
                0 -- TimePassed
                ]

            writeBuffer pointLightUniformBuffer 0 []

            render $ do
                let shadowTex = frameBufferGroupShadowTex frameBufferGroup
                    shadowTexSize = texture2DSizes shadowTex !! 0
                sImage <- getTexture2DImage shadowTex 0
                clearImageDepth sImage 1
                primArray <- mconcat <$> mapM (fmap (toPrimitiveArray TriangleList) . newVertexArray) buffers
                compiledShadowShader $ ShadowShaderEnvironment primArray sImage (Front, ViewPort (V2 0 0) shadowTexSize, DepthRange 0 1)

            return shadowMat

        renderObjects shadowMat bounds frameBufferGroup camera sun lights buffers = do
            let
                depthTex = frameBufferGroupDepthTex frameBufferGroup
                positionTex = frameBufferGroupPositionTex frameBufferGroup
                normalTex = frameBufferGroupNormalTex frameBufferGroup
                materialTex = frameBufferGroupMaterialTex frameBufferGroup
                occlusionTex = frameBufferGroupOcclusionTex frameBufferGroup
                shadowTex = frameBufferGroupShadowTex frameBufferGroup
                blurredOcclusionTex = frameBufferGroupBlurredOcclusionTex frameBufferGroup

                ((x, y), (w, h)) = bounds
                -- FOV (y direction, in radians), Aspect ratio, Near plane, Far plane
                projectionMat = perspective (cameraFov camera) (fromIntegral w / fromIntegral h) near far
                -- Eye, Center, Up
                cameraMat = lookAt
                    (cameraPosition camera)
                    (cameraPosition camera + getSight camera)
                    (getUp camera)

            writeBuffer shaderConfigUniformBuffer 0 [ShaderConfig
                (cameraPosition camera)
                projectionMat
                cameraMat
                identity -- Transformation
                0 -- ShadowUsed
                shadowMat
                (Fog (V4 0.5 0.5 0.5 1) 10 100 0.2)
                sun
                0 -- TimePassed
                ]

            writeBuffer pointLightUniformBuffer 0 lights

            let viewPort = ViewPort (V2 x y) (V2 w h)

            -- See https://github.com/tobbebex/GPipe-Core/issues/50

            -- Direct rendering
            render $ do
                clearWindowDepth window 1
                primArray <- mconcat <$> mapM (fmap (toPrimitiveArray TriangleList) . newVertexArray) buffers
                compiledDirectShader $ DirectShaderEnvironment primArray shadowTex (Front, viewPort, DepthRange 0 1)

            -- Deferred rendering
            render $ do
                clearWindowDepth window 1
                dImage <- getTexture2DImage depthTex 0
                pImage <- getTexture2DImage positionTex 0
                nImage <- getTexture2DImage normalTex 0
                aImage <- getTexture2DImage materialTex 0
                clearImageDepth dImage 1
                clearImageColor pImage 0
                clearImageColor nImage 0
                clearImageColor aImage 0
                primArray <- mconcat <$> mapM (fmap (toPrimitiveArray TriangleList) . newVertexArray) buffers
                compiledDeferredShader $ DeferredShaderEnvironment primArray dImage pImage nImage aImage (Front, viewPort, DepthRange 0 1)
                
            -- SSAO
            render $ do
                oImage <- getTexture2DImage occlusionTex 0
                clearImageDepth oImage 1
                screenPrimArray <- toPrimitiveArray TriangleList <$> newVertexArray screenBuffer
                compiledSsaoShader $ SsaoShaderEnvironment screenPrimArray oImage positionTex normalTex sampleKernelTex noiseTex (Front, viewPort, DepthRange 0 1)
                
            -- Bluring
            render $ do
                bImage <- getTexture2DImage blurredOcclusionTex 0
                clearImageDepth bImage 1
                screenPrimArray <- toPrimitiveArray TriangleList <$> newVertexArray screenBuffer
                compiledBlurShader $ BlurShaderEnvironment screenPrimArray bImage occlusionTex (Front, viewPort, DepthRange 0 1)
                
            -- Debug
            render $ do
                screenPrimArray <- toPrimitiveArray TriangleList <$> newVertexArray screenBuffer
                compiledScreenShader $ ScreenShaderEnvironment screenPrimArray occlusionTex (Front, viewPort, DepthRange 0 1)

            -- Lighting
            -- (Copy back the depth buffer from our initial rendering.)
            -- (Render other objects (especially transparent ones) directly.)
            render $ do
                screenPrimArray <- toPrimitiveArray TriangleList <$> newVertexArray screenBuffer
                compiledLightingShader $ LightingShaderEnvironment
                    screenPrimArray
                        shadowTex
                        positionTex
                        normalTex
                        materialTex
                        blurredOcclusionTex
                        (Front, viewPort, DepthRange 0 1)

            -- Tone mapping

            -- Gaussian blur

            -- Combine


            -- Post (direct rendering)
            render $ do
                gridPrimArray <- toPrimitiveArray TriangleList <$> newVertexArray gridBuffer
                compiledGridShader $ GridShaderEnvironment gridPrimArray (FrontAndBack, viewPort, DepthRange 0 1)

    return $ RenderContext Nothing renderAction

data ShadowShaderEnvironment = ShadowShaderEnvironment
    { shadowPrimitives :: PrimitiveArray Triangles (B3 Float, B3 Float)
    , shadowImage :: Image (Format Depth)
    , shadowRasterOptions :: (Side, ViewPort, DepthRange)
    }

shadowShader :: Window os RGBAFloat Depth -> Buffer os (Uniform ShaderConfigB) -> Shader os ShadowShaderEnvironment ()
shadowShader _ shaderConfigUniformBuffer = do
    config <- getUniform (const (shaderConfigUniformBuffer, 0))
    let viewProjMat = shaderConfigProjectionS config !*! shaderConfigCameraS config !*! shaderConfigTransformationS config

    triangles :: PrimitiveStream Triangles (V3 VFloat, V3 VFloat) <- toPrimitiveStream shadowPrimitives
    let
        projectedTriangles :: PrimitiveStream Triangles (V4 VFloat, ())
        projectedTriangles = (\(p, _) -> (viewProjMat !* v3To4 p 1, ())) <$> triangles

    frags :: FragmentStream () <- rasterize shadowRasterOptions projectedTriangles
    let
        shadowFragsWithDepth = flip withRasterizedInfo frags $ \_ p ->
            (undefined, let (V2 z w) = rasterizedFragCoord p ^. _zw in z/w)
        depthOption = DepthOption Less True

    drawDepth (\env -> (NoBlending, shadowImage env, depthOption)) shadowFragsWithDepth (const (return ()))

data DirectShaderEnvironment os = DirectShaderEnvironment
    { directShaderPrimitives :: PrimitiveArray Triangles (B3 Float, B3 Float)
    , directShaderShadowTex :: Texture2D os (Format Depth)
    , directShaderRasterOptions :: (Side, ViewPort, DepthRange)
    }

directShader :: Window os RGBAFloat Depth -> Buffer os (Uniform ShaderConfigB) -> Shader os (DirectShaderEnvironment os) ()
directShader window shaderConfigUniformBuffer = do
    config <- getUniform (const (shaderConfigUniformBuffer, 0))
    let viewProjMat = shaderConfigProjectionS config !*! shaderConfigCameraS config !*! shaderConfigTransformationS config
        camPos = shaderConfigCameraPositionS config
        normMat = (shaderConfigTransformationS config)^._m33
        shadowMat = shaderConfigShadowS config
        lightingContext =
            ( camPos
            , shaderConfigFogS config
            , shaderConfigSunS config
            , 0.8 -- material specular intensity
            , 8 -- material specular power
            )

    triangles :: PrimitiveStream Triangles (V3 VFloat, V3 VFloat) <- toPrimitiveStream directShaderPrimitives
    let
        projWithShadow (position, normal) =
            let p = v3To4 position 1
            -- TODO position is not transformed
            in  (viewProjMat !* p, (normMat !* normal, shadowMat !* p, position, lightingContext))

        projectedTriangles :: PrimitiveStream Triangles (V4 VFloat, (V3 VFloat, V4 VFloat, V3 VFloat, LightingContext V))
        projectedTriangles = projWithShadow <$> triangles

    let samplerFilter = SamplerFilter Nearest Nearest Nearest Nothing
        edge = (pure ClampToEdge, 0)
    sampler <- newSampler2D (\env -> (directShaderShadowTex env, samplerFilter, edge))

    fragNormals :: FragmentStream (V3 FFloat, V4 FFloat, V3 FFloat, LightingContext F) <- rasterize directShaderRasterOptions projectedTriangles
    let
        sampleTexture = sample2D sampler SampleAuto Nothing Nothing
        litFrags = simpleLightWithShadow sampleTexture <$> fragNormals
        litFragsWithDepth = withRasterizedInfo
            (\a p -> (a, rasterizedFragCoord p ^. _z)) litFrags
        colorOption = ContextColorOption NoBlending (pure True)
        depthOption = DepthOption Less True

    drawWindowColorDepth (const (window, colorOption, depthOption)) litFragsWithDepth

data DeferredShaderEnvironment os = DeferredShaderEnvironment
    { deferredShaderPrimitives :: PrimitiveArray Triangles (B3 Float, B3 Float)
    , deferredShaderDepthImage :: Image (Format Depth)
    , deferredShaderPositionImage :: Image (Format RGBFloat)
    , deferredShaderNormalImage :: Image (Format RGBFloat)
    , deferredShaderMaterialImage :: Image (Format RGBAFloat)
    , deferredShaderRasterOptions :: (Side, ViewPort, DepthRange)
    }

deferredShader :: Window os RGBAFloat Depth -> Buffer os (Uniform ShaderConfigB) -> Shader os (DeferredShaderEnvironment os) ()
deferredShader _ shaderConfigUniformBuffer = do
    config <- getUniform (const (shaderConfigUniformBuffer, 0))
    let viewProjMat = shaderConfigProjectionS config !*! shaderConfigCameraS config !*! shaderConfigTransformationS config

    triangles :: PrimitiveStream Triangles (V3 VFloat, V3 VFloat) <- toPrimitiveStream deferredShaderPrimitives
    let
        projGeometry (position, normal) =
            let p = v3To4 position 1
            -- TODO position is not transformed
            in  (viewProjMat !* p, (position, normal, V4 0.7 0.7 0.7 1.0))

        projectedTriangles :: PrimitiveStream Triangles (V4 VFloat, (V3 VFloat, V3 VFloat, V4 VFloat))
        projectedTriangles = projGeometry <$> triangles

    geometryFrags :: FragmentStream (V3 FFloat, V3 FFloat, V4 FFloat) <- rasterize deferredShaderRasterOptions projectedTriangles
    let
        geometryFragsWithDepth = withRasterizedInfo
            (\a p -> (a, rasterizedFragCoord p ^. _z)) geometryFrags
        depthOption = DepthOption Less True

    drawDepth (\env -> (NoBlending, deferredShaderDepthImage env, depthOption)) geometryFragsWithDepth $ \(p, n ,c) -> do
        drawColor (\env -> (deferredShaderPositionImage env, pure True, False)) p
        drawColor (\env -> (deferredShaderNormalImage env, pure True, False)) n
        drawColor (\env -> (deferredShaderMaterialImage env, pure True, False)) c

data SsaoShaderEnvironment os = SsaoShaderEnvironment
    { screenToDepthPrimitives :: PrimitiveArray Triangles (B2 Float)
    , screenToDepthDepthImage :: Image (Format Depth)
    , screenPositionTex :: Texture2D os (Format RGBFloat)
    , screenNormalTex :: Texture2D os (Format RGBFloat)
    , screenKernelTex :: Texture1D os (Format RGBFloat)
    , screenNoiseTex :: Texture2D os (Format RGBFloat)
    , screenToDepthRasterOptions :: (Side, ViewPort, DepthRange)
    }

ssaoShader :: Window os RGBAFloat Depth -> Buffer os (Uniform ShaderConfigB) -> Shader os (SsaoShaderEnvironment os) ()
ssaoShader _ shaderConfigUniformBuffer = do
    config <- getUniform (const (shaderConfigUniformBuffer, 0))
    let viewProjMat = shaderConfigProjectionS config
        cameraMat = shaderConfigCameraS config !*! shaderConfigTransformationS config

    triangles :: PrimitiveStream Triangles (V4 VFloat, ()) <- do
        coords <- toPrimitiveStream screenToDepthPrimitives
        return $ (\(V2 x y) -> (V4 x y 0 1, ())) <$> coords

    let createSampler2D tex =
            let samplerFilter = SamplerFilter Nearest Nearest Nearest Nothing
                edge = (pure ClampToEdge, 0)
            in  newSampler2D (\env -> (tex env, samplerFilter, edge))

    let createSampler1D tex =
            let samplerFilter = SamplerFilter Nearest Nearest Nearest Nothing
                edge = (ClampToEdge, 0)
            in  newSampler1D (\env -> (tex env, samplerFilter, edge))

    let createNoiseSampler2D tex =
            let samplerFilter = SamplerFilter Nearest Nearest Nearest Nothing
                edge = (pure Repeat, 0)
            in  newSampler2D (\env -> (tex env, samplerFilter, edge))

    positionSampler <- createSampler2D screenPositionTex
    normalSampler <- createSampler2D screenNormalTex
    kernelSampler <- createSampler1D screenKernelTex
    noiseSampler <- createNoiseSampler2D screenNoiseTex

    frags :: FragmentStream () <- rasterize screenToDepthRasterOptions triangles
    let
        positionSample = sample2D positionSampler SampleAuto Nothing Nothing
        normalSample = sample2D normalSampler SampleAuto Nothing Nothing
        kernelSample = sample1D kernelSampler SampleAuto Nothing Nothing
        noiseSample = sample2D noiseSampler SampleAuto Nothing Nothing

        occlusionFrags = withRasterizedInfo getOcclusion frags

        V2 screenWidth screenHeight = toFloat <$> sampler2DSize positionSampler 0
        V2 noiseWidth noiseHeight = toFloat <$> sampler2DSize noiseSampler 0

        getOcclusion _ f = occlusion where
            V4 x y _ _ = rasterizedFragCoord f
            texCoords = V2 (x / screenWidth) (y / screenHeight)
            noiseScale = V2 (screenWidth / noiseWidth) (screenHeight / noiseHeight)

            noise = noiseSample (texCoords * noiseScale)

            position = cameraMat !* v3To4 (positionSample texCoords) 1
            normal = v4To3 $ cameraMat !* v3To4 (normalSample texCoords) 0

            tangent = signorm (noise - normal ^* dot noise normal)
            bitangent = cross normal tangent
            tbn = V3 tangent bitangent normal

            kernelTexSize = sampler1DSize kernelSampler 0

            contributions = snd $ while ((0 <*) . fst) getContribution (kernelTexSize, 0)

            getContribution (i, acc) = (i - 1, acc + contribution) where

                sample = kernelSample (toFloat i / toFloat kernelTexSize)

                radius = 0.25
                bias = 0.001

                -- From tangent to view-space
                samplePosition = position + v3To4 ((tbn !* sample) ^* radius) 0

                -- From view to clip-space + perspective divide + to range [0, 1]
                texCoords' = (v4To3' (viewProjMat !* samplePosition) * 0.5 + 0.5)^._xy

                position' = cameraMat !* v3To4 (positionSample texCoords') 1

                -- rangeCheck = smoothstep 0 1 (radius / abs (position^._z - position'^._z))
                -- rangeCheck = smoothstep 0 1 (abs (position^._z - position'^._z))
                rangeCheck = 1

                contribution = (ifThenElse' (position'^._z >=* samplePosition^._z + bias) 1 0) * rangeCheck

            occlusion = 1 - contributions / toFloat kernelTexSize

    let
        fragsWithDepth = withRasterizedInfo
            (\a _ -> (a, a)) occlusionFrags
        depthOption = DepthOption Always True

    -- TODO Switch to color instead?
    drawDepth (\env -> (NoBlending, screenToDepthDepthImage env, depthOption)) fragsWithDepth
        (const $ return ())

generateSampleKernel :: Int -> IO [V3 Float]
generateSampleKernel size = forM [0 .. size-1] $ \i -> do
    [x, y, z] <- replicateM 3 (runRandomIO $ getRandomR (0, 1)) :: IO [Float]
    let lerp a b f = a + f * (b - a)
        s = fromIntegral i / fromIntegral size
        sample = lerp 0.1 1 (s * s) *^ normalize (V3 (x * 2 - 1) (y * 2 - 1) z)
    return sample

generateSampleKernelTexture :: Int -> ContextT GLFW.Handle os IO (Texture1D os (Format RGBFloat))
generateSampleKernelTexture size = do
    sampleKernel <- liftIO (generateSampleKernel size)
    texture <- newTexture1D RGB16F size maxBound
    writeTexture1D texture 0 0 size sampleKernel
    return texture

data BlurShaderEnvironment os = BlurShaderEnvironment
    { blurPrimitives :: PrimitiveArray Triangles (B2 Float)
    , blurDepthImage :: Image (Format Depth)
    , blurDepthTex :: Texture2D os (Format Depth)
    , blurRasterOptions :: (Side, ViewPort, DepthRange)
    }

blurShader :: Window os RGBAFloat Depth -> Shader os (BlurShaderEnvironment os) ()
blurShader _ = do
    triangles :: PrimitiveStream Triangles (V4 VFloat, ()) <- do
        texCoords <- toPrimitiveStream blurPrimitives
        return $ (\(V2 x y) -> (V4 x y 0 1, ())) <$> texCoords

    let createSampler2D tex =
            let samplerFilter = SamplerFilter Nearest Nearest Nearest Nothing
                edge = (pure ClampToEdge, 0)
            in  newSampler2D (\env -> (tex env, samplerFilter, edge))

    depthSampler <- createSampler2D blurDepthTex

    let texelSize@(V2 w h) = toFloat <$> sampler2DSize depthSampler 0
        r :: Int = 2

    frags :: FragmentStream () <- rasterize blurRasterOptions triangles
    let
        depthSample = sample2D depthSampler SampleAuto Nothing Nothing
        blurredFrags = withRasterizedInfo blur frags
        blur c f =
            let (V4 x y _ _) = rasterizedFragCoord f
                texCoords = V2 (x / w) (y / h)
                offsets = [ (fromIntegral <$> V2 x y) / texelSize | x <- [-r .. r], y <- [-r .. r]]
                averageDepth = sum (map (\offset -> depthSample (texCoords + offset)) offsets) / fromIntegral ((2*r+1)^2)
            in  (c, averageDepth)

        depthOption = DepthOption Always True

    drawDepth (\env -> (NoBlending, blurDepthImage env, depthOption)) blurredFrags (const (return ()))

data ScreenShaderEnvironment os = ScreenShaderEnvironment
    { screenPrimitives :: PrimitiveArray Triangles (B2 Float)
    , screenDepthTex :: Texture2D os (Format Depth)    
    , screenRasterOptions :: (Side, ViewPort, DepthRange)
    }

screenShader :: Window os RGBAFloat Depth -> Shader os (ScreenShaderEnvironment os) ()
screenShader window = do
    triangles :: PrimitiveStream Triangles (V4 VFloat, ()) <- do
        texCoords <- toPrimitiveStream screenPrimitives
        return $ (\(V2 x y) -> (V4 x y 0 1, ())) <$> texCoords

    let createSampler2D tex =
            let samplerFilter = SamplerFilter Nearest Nearest Nearest Nothing
                edge = (pure ClampToEdge, 0)
            in  newSampler2D (\env -> (tex env, samplerFilter, edge))

    depthSampler <- createSampler2D screenDepthTex

    frags :: FragmentStream () <- rasterize screenRasterOptions triangles
    let
        depthSample = sample2D depthSampler SampleAuto Nothing Nothing
        colorFrags = withRasterizedInfo (\_ f -> pickDepthColor f) frags
        V2 w h = toFloat <$> sampler2DSize depthSampler 0
        pickDepthColor f =
            let (V4 x y _ _) = rasterizedFragCoord f
                texCoords = V2 (x / w) (y / h)
                d = depthSample texCoords
            in  (V4 d d d 1)

        colorOption = ContextColorOption NoBlending (pure True)

    drawWindowColor (const (window, colorOption)) colorFrags

data LightingShaderEnvironment os = LightingShaderEnvironment
    { lightingShaderPrimitives :: PrimitiveArray Triangles (B2 Float)
    , lightingShaderShadowTex :: Texture2D os (Format Depth)
    , lightingScreenPositionTex :: Texture2D os (Format RGBFloat)
    , lightingScreenNormalTex :: Texture2D os (Format RGBFloat)
    , lightingScreenMaterialTex :: Texture2D os (Format RGBAFloat)
    , lightingShaderOcclusionTex :: Texture2D os (Format Depth)
    , lightingShaderRasterOptions :: (Side, ViewPort, DepthRange)
    }

lightingShader :: Window os RGBAFloat Depth -> Buffer os (Uniform ShaderConfigB) -> Shader os (LightingShaderEnvironment os) ()
lightingShader window shaderConfigUniformBuffer = do
    config <- getUniform (const (shaderConfigUniformBuffer, 0))
    let viewProjMat = shaderConfigProjectionS config !*! shaderConfigCameraS config !*! shaderConfigTransformationS config
        camPos = shaderConfigCameraPositionS config
        normMat = (shaderConfigTransformationS config)^._m33
        shadowMat = shaderConfigShadowS config
        lightingContext =
            ( camPos
            , shaderConfigFogS config
            , shaderConfigSunS config
            , 0.8 -- material specular intensity
            , 8 -- material specular power
            )

    triangles :: PrimitiveStream Triangles (V4 VFloat, ()) <- do
        texCoords <- toPrimitiveStream lightingShaderPrimitives
        return $ (\(V2 x y) -> (V4 x y 0 1, ())) <$> texCoords

    let createSampler2D tex =
            let samplerFilter = SamplerFilter Nearest Nearest Nearest Nothing
                edge = (pure ClampToEdge, 0)
            in  newSampler2D (\env -> (tex env, samplerFilter, edge))

    shadowSampler <- createSampler2D lightingShaderShadowTex
    positionSampler <- createSampler2D lightingScreenPositionTex
    normalSampler <- createSampler2D lightingScreenNormalTex
    materialSampler <- createSampler2D lightingScreenMaterialTex
    occlusionSampler <- createSampler2D lightingShaderOcclusionTex

    frags :: FragmentStream () <- rasterize lightingShaderRasterOptions triangles
    let
        shadowSample = sample2D shadowSampler SampleAuto Nothing Nothing
        positionSample = sample2D positionSampler SampleAuto Nothing Nothing
        normalSample = sample2D normalSampler SampleAuto Nothing Nothing
        materialSample = sample2D materialSampler SampleAuto Nothing Nothing
        occlusionSample = sample2D occlusionSampler SampleAuto Nothing Nothing

        V2 w h = toFloat <$> sampler2DSize positionSampler 0

        litFragsWithDepth = (flip withRasterizedInfo) frags $ \_ f ->
            let (V4 x y _ _) = rasterizedFragCoord f
                texCoords = V2 (x / w) (y / h)
                position = positionSample texCoords
                normal = normalSample texCoords
                _ = materialSample texCoords
                occlusion = occlusionSample texCoords
                lighting = lightWithShadow
                    shadowSample
                    (normMat !* normal)
                    (shadowMat !* v3To4 position 1)
                    position
                    occlusion
                    lightingContext
                p = viewProjMat !* v3To4 position 1
            in  (lighting, 0)

        colorOption = ContextColorOption NoBlending (pure True)
        depthOption = DepthOption Less True

    drawWindowColorDepth (const (window, colorOption, depthOption)) litFragsWithDepth

data GridShaderEnvironment = GridShaderEnvironment
    { gridPrimitives :: PrimitiveArray Triangles (B3 Float)
    , gridRasterOptions :: (Side, ViewPort, DepthRange)
    }

gridShader :: Window os RGBAFloat Depth -> Buffer os (Uniform ShaderConfigB) -> Shader os GridShaderEnvironment ()
gridShader window shaderConfigUniformBuffer = do
    config <- getUniform (const (shaderConfigUniformBuffer, 0))
    let viewProjMat = shaderConfigProjectionS config !*! shaderConfigCameraS config !*! shaderConfigTransformationS config
        camPos = shaderConfigCameraPositionS config

    triangles :: PrimitiveStream Triangles (V3 VFloat) <- toPrimitiveStream gridPrimitives
    let
        projectedTriangles :: PrimitiveStream Triangles (V4 VFloat, (V2 VFloat, VFloat, FogS V))
        projectedTriangles =
            (\p -> (viewProjMat !* p, (p^._xy, camPos^._z, shaderConfigFogS config))) .
            (\p -> v3To4 p 1).
            (* 4000) <$> triangles

    fragCoord :: FragmentStream (V2 FFloat, FFloat, FogS F) <- rasterize gridRasterOptions projectedTriangles
    let
        drawGridLine :: (V2 FFloat, FFloat, FogS F) -> V4 FFloat
        drawGridLine (p, camPosZ, fog) = color' where
            -- Pick a coordinate to visualize in a grid
            coord = p / 40
            -- Compute anti-aliased renderContext-space grid lines
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

type LightingContext x =
    ( V3 (S x Float) -- camPos
    , FogS x
    , DirectionLightS x -- sun
    , S x Float -- material specular intensity
    , S x Float -- material specular power
    )

simpleLightWithShadow :: (V2 FFloat -> ColorSample F Depth)
    -> (V3 FFloat, V4 FFloat, V3 FFloat, LightingContext F)
    -> V4 FFloat
simpleLightWithShadow shadowSample (normal, shadowCoord, renderContextSpacePosition, lightingContext) =
    let (cameraPosition, fog, sun, specularIntensity, specularPower) = lightingContext
        DirectionLightS sunLightColor sunLightDirection sunLightAmbientIntensity = sun
        cameraDirection = signorm (cameraPosition - renderContextSpacePosition)
        baseColor = pure 1 -- V4 0.4 0.4 0.4 1

        -- Sun
        ambient = sunLightColor ^* sunLightAmbientIntensity
        diffuse = sunLightColor ^* maxB 0 (dot normal (-sunLightDirection))
        specular = getSpecularColor cameraDirection normal specularIntensity specularPower (sunLightColor * 1.5) (-sunLightDirection)
        shadow = getShadow shadowSample normal (-sunLightDirection) shadowCoord
        sunContribution = ambient + (diffuse + specular) ^* shadow

        -- Lights
        lightContributions = pure 0
        {-
        lights :: [PointLight] = []
        lightContributions = sum $ flip map lights $ \(lightPosition, lightColor) ->
            lightDirection = normalize (lightPosition - renderContextSpacePosition)
            lightDirection = length (lightPosition - renderContextSpacePosition)
            attenuation = 0.01 + 0.07 * lightDistance + 0.00008 * lightDistance * lightDistance

            diffuse = lightColor * maxB 0 (dot normal lightDirection)
            specular = getSpecularColor cameraDirection normal specularIntensity specularPower lightColor lightDirection
            baseColor * (diffuse + specular) / attenuation
        -}

    in  baseColor * (v3To4 sunContribution 1 + lightContributions)

lightWithShadow :: (V2 FFloat -> ColorSample F Depth)
    -> V3 FFloat
    -> V4 FFloat
    -> V3 FFloat
    -> FFloat
    -> LightingContext F
    -> V4 FFloat
lightWithShadow shadowSample normal shadowCoord renderContextSpacePosition occlusion lightingContext =
    let (cameraPosition, fog, sun, specularIntensity, specularPower) = lightingContext
        DirectionLightS sunLightColor sunLightDirection sunLightAmbientIntensity = sun
        cameraDirection = signorm (cameraPosition - renderContextSpacePosition)
        baseColor = pure 1 -- V4 0.4 0.4 0.4 1

        -- Sun
        ambient = sunLightColor ^* sunLightAmbientIntensity
        diffuse = sunLightColor ^* maxB 0 (dot normal (-sunLightDirection))
        specular = getSpecularColor cameraDirection normal specularIntensity specularPower (sunLightColor * 1.5) (-sunLightDirection)
        shadow = getShadow shadowSample normal (-sunLightDirection) shadowCoord
        sunContribution = ambient + (diffuse + specular) ^* shadow

        -- Lights
        lightContributions = pure 0
        {-
        lights :: [PointLight] = []
        lightContributions = sum $ flip map lights $ \(lightPosition, lightColor) ->
            lightDirection = normalize (lightPosition - renderContextSpacePosition)
            lightDirection = length (lightPosition - renderContextSpacePosition)
            attenuation = 0.01 + 0.07 * lightDistance + 0.00008 * lightDistance * lightDistance

            diffuse = lightColor * maxB 0 (dot normal lightDirection)
            specular = getSpecularColor cameraDirection normal specularIntensity specularPower lightColor lightDirection
            baseColor * (diffuse + specular) / attenuation
        -}

    in  v3To4 (baseColor * (sunContribution + lightContributions) * pure occlusion) 1

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
getShadow shadowSample normal sunLightDirection (V4 x y z w) = maxB 0 visibility where
    rectify c = (c/w + 1) / 2
    texCoords = V2 (rectify x) (rectify y)

    cosTheta = clamp (dot normal sunLightDirection) 0 1
    bias = clamp (0.005 * tan (acos cosTheta)) 0 0.01

    sample offset = shadowSample (texCoords + offset / 700)
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

v3To4 :: Floating a => V3 a -> a -> V4 a
v3To4 (V3 x y z) w = V4 x y z w

v4To3 :: Floating a => V4 a -> V3 a
v4To3 (V4 x y z w) = V3 x y z

v4To3' :: Floating a => V4 a -> V3 a
v4To3' (V4 x y z w) = V3 (x/w) (y/w) (z/w)
