{-# language Arrows, FlexibleInstances, UndecidableInstances, ScopedTypeVariables, TypeFamilies, FlexibleContexts, RankNTypes #-}

module Graphics.Shaders
    ( Gui (..)
    , DirectionLight (..), DirectionLightB (..), DirectionLightS (..)
    , PointLight (..), PointLightB (..), PointLightS (..)
    , FogEquation(..)
    , Fog(..) , FogB(..), FogS(..), applyFog
    , ShaderConfig (..), ShaderConfigB (..), ShaderConfigS (..)
    , FrameBufferGroup(..)
    , RenderContext(..)
    , createFrameBufferGroup
    , ShadowShaderEnvironment(..)
    , shadowShader
    , DirectShaderEnvironment(..)
    , directShader
    , DeferredShaderEnvironment(..)
    , deferredShader
    , SsaoShaderEnvironment(..)
    , ssaoShader
    , generateSampleKernelTexture
    , BlurShaderEnvironment(..)
    , blurShader
    , LightingShaderEnvironment(..)
    , lightingShader
    , GridShaderEnvironment(..)
    , gridShader
    , ScreenShaderEnvironment(..)
    , screenShader
    , NormalShaderEnvironment(..)
    , normalShader
    , LightingContext
    , getSunlight
    , getLight
    , getFogFactor
    , getSpecularColor
    , reflect
    , getShadow
    , poissonDisk
    ) where

import Prelude hiding ((<*))

import Control.Applicative (pure)
import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Exception
import Control.Lens
import Data.Int
import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import Linear

import Graphics.Geometry
import Graphics.Texture
import Common.Random

------------------------------------------------------------------------------------------------------------------------

data Gui = Gui
    { guiFps :: Double
    , guiFogDensity :: Float
    , guiDebug :: Bool
    }

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

data FogEquation = FogLinear | FogExp | FogExp2
    deriving (Show)

data Fog = Fog
    { fogColor :: !(V4 Float)
    , fogStart :: !Float
    , fogEnd :: !Float
    , fogDensity :: !Float
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
    { shaderConfigViewPortOffset :: !(V2 Float)

    , shaderConfigCameraPosition :: !(V3 Float)

    , shaderConfigProjection :: !(M44 Float)
    , shaderConfigCamera :: !(M44 Float)
    , shaderConfigTransformation :: !(M44 Float)

    , shaderConfigShadowUsed :: !Int32
    , shaderConfigShadow :: !(M44 Float)

    , shaderConfigFog :: Fog
    , shaderConfigSun :: DirectionLight

    , shaderConfigTimePassed :: !Float
    }

data ShaderConfigB = ShaderConfigB
    { shaderConfigViewPortOffsetB :: !(B2 Float)

    , shaderConfigCameraPositionB :: !(B3 Float)

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
    toBuffer = proc ~(ShaderConfig a b c d e f g h i j) -> do
            ((a', b', c', d', e', f'), (g', h', i', j')) <- toBuffer -< ((a, b, c, d, e, f), (g, h, i, j))
            returnA -< (ShaderConfigB a' b' c' d' e' f' g' h' i' j')

data ShaderConfigS x = ShaderConfigS
    { shaderConfigViewPortOffsetS :: !(V2 (S x Float))

    , shaderConfigCameraPositionS :: !(V3 (S x Float))

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
    toUniform = proc ~(ShaderConfigB a b c d e f g h i j) -> do
            ((a', b', c', d', e', f'), (g', h', i', j')) <- toUniform -< ((a, b, c, d, e, f), (g, h, i, j))
            returnA -< (ShaderConfigS a' b' c' d' e' f' g' h' i' j')

instance VertexInput ShaderConfig where
    type VertexFormat ShaderConfig = ShaderConfigS V
    toVertex = proc ~(ShaderConfig a b c d e f g h i j) -> do
            ((a', b', c', d', e', f'), (g', h', i', j')) <- toVertex -< ((a, b, c, d, e, f), (g, h, i, j))
            returnA -< (ShaderConfigS a' b' c' d' e' f' g' h' i' j')

instance FragmentInput (ShaderConfigS V) where
    type FragmentFormat (ShaderConfigS V) = ShaderConfigS F
    toFragment = proc ~(ShaderConfigS a b c d e f g h i j) -> do
            ((a', b', c', d', e', f'), (g', h', i', j')) <- toFragment -< ((a, b, c, d, e, f), (g, h, i, j))
            returnA -< (ShaderConfigS a' b' c' d' e' f' g' h' i' j')

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

data RenderContext m os = RenderContext
    { renderContextFrameBufferGroup :: Maybe (Size2, FrameBufferGroup os)
    , renderContextRenderAction :: (MonadIO m, MonadAsyncException m)
        => RenderContext m os
        -> ((Int, Int), (Int, Int))
        -> Camera
        -> [Camera]
        -> DirectionLight
        -> [PointLight]
        -> [Buffer os (B3 Float, B3 Float)]
        -> [Buffer os (B3 Float)]
        -> Gui
        -> ContextT GLFW.Handle os m (RenderContext m os)
    }

createFrameBufferGroup :: (MonadIO m, MonadAsyncException m) => Size2 -> ContextT GLFW.Handle os m (Size2, FrameBufferGroup os)
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

data ShadowShaderEnvironment = ShadowShaderEnvironment
    { shadowPrimitives :: PrimitiveArray Triangles (B3 Float, B3 Float)
    , shadowImage :: Image (Format Depth)
    }

shadowShader :: Window os RGBAFloat Depth -> Buffer os (Uniform ShaderConfigB) -> Shader os ShadowShaderEnvironment ()
shadowShader _ shaderConfigUniformBuffer = do
    config <- getUniform (const (shaderConfigUniformBuffer, 0))
    let viewProjMat = shaderConfigProjectionS config !*! shaderConfigCameraS config !*! shaderConfigTransformationS config

    triangles :: PrimitiveStream Triangles (V3 VFloat, V3 VFloat) <- toPrimitiveStream shadowPrimitives
    let
        projectedTriangles :: PrimitiveStream Triangles (V4 VFloat, ())
        projectedTriangles = (\(p, _) -> (viewProjMat !* point p, ())) <$> triangles

        getRasterOptions env = (FrontAndBack, ViewPort (V2 0 0) (imageSize (shadowImage env)), DepthRange 0 1)

    frags :: FragmentStream () <- rasterize getRasterOptions projectedTriangles
    let
        shadowFragsWithDepth = flip withRasterizedInfo frags $ \_ p ->
            (undefined, let (V2 z w) = rasterizedFragCoord p ^. _zw in z/w)
        depthOption = DepthOption Less True

    drawDepth (\env -> (NoBlending, shadowImage env, depthOption)) shadowFragsWithDepth (const (return ()))

-- (Front, viewPort, DepthRange 0 1)
data DirectShaderEnvironment os = DirectShaderEnvironment
    { directShaderViewport :: ViewPort
    , directShaderPrimitives :: PrimitiveArray Triangles (B3 Float, B3 Float)
    , directShaderShadowTex :: Texture2D os (Format Depth)
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
            let p = point position
            -- TODO position is not transformed
            in  (viewProjMat !* p, (normMat !* normal, shadowMat !* p, position, lightingContext))

        projectedTriangles :: PrimitiveStream Triangles (V4 VFloat, (V3 VFloat, V4 VFloat, V3 VFloat, LightingContext V))
        projectedTriangles = projWithShadow <$> triangles

        getRasterOptions env = (Front, directShaderViewport env, DepthRange 0 1)

    sampler <- newSampler2D $ \env ->
        ( directShaderShadowTex env
        , SamplerFilter Nearest Nearest Nearest Nothing
        , (pure ClampToEdge, 0)
        )

    fragNormals :: FragmentStream (V3 FFloat, V4 FFloat, V3 FFloat, LightingContext F) <- rasterize getRasterOptions projectedTriangles
    let
        sampleTexture = sample2D sampler SampleAuto Nothing Nothing
        lightWithShadow' (normal, shadowCoord, renderContextSpacePosition, lightingContext) =
            getSunlight sampleTexture normal (Just shadowCoord) renderContextSpacePosition (pure 1) 1 lightingContext
        litFrags = lightWithShadow' <$> fragNormals
        litFragsWithDepth = withRasterizedInfo
            (\a p -> (a, rasterizedFragCoord p ^. _z)) litFrags
        colorOption = ContextColorOption NoBlending (pure True)
        depthOption = DepthOption Less True

    drawWindowColorDepth (const (window, colorOption, depthOption)) litFragsWithDepth

data DeferredShaderEnvironment os = DeferredShaderEnvironment
    { deferredShaderViewport :: ViewPort
    , deferredShaderPrimitives :: PrimitiveArray Triangles (B3 Float, B3 Float)
    , deferredShaderDepthImage :: Image (Format Depth)
    , deferredShaderPositionImage :: Image (Format RGBFloat)
    , deferredShaderNormalImage :: Image (Format RGBFloat)
    , deferredShaderMaterialImage :: Image (Format RGBAFloat)
    }

deferredShader :: Window os RGBAFloat Depth -> Buffer os (Uniform ShaderConfigB) -> Shader os (DeferredShaderEnvironment os) ()
deferredShader window shaderConfigUniformBuffer = do
    config <- getUniform (const (shaderConfigUniformBuffer, 0))
    let viewProjMat = shaderConfigProjectionS config !*! shaderConfigCameraS config !*! shaderConfigTransformationS config

    triangles :: PrimitiveStream Triangles (V3 VFloat, V3 VFloat) <- toPrimitiveStream deferredShaderPrimitives
    let
        projGeometry (position, normal) =
            let p = point position
            -- TODO position is not transformed
            in  (viewProjMat !* p, (position, normal, pure 1))

        projectedTriangles :: PrimitiveStream Triangles (V4 VFloat, (V3 VFloat, V3 VFloat, V4 VFloat))
        projectedTriangles = projGeometry <$> triangles

        getRasterOptions env = (Front, deferredShaderViewport env, DepthRange 0 1)

    geometryFrags :: FragmentStream (V3 FFloat, V3 FFloat, V4 FFloat) <- rasterize getRasterOptions projectedTriangles
    let
        geometryFragsWithDepth = withRasterizedInfo
            (\a p -> (a, rasterizedFragCoord p ^. _z)) geometryFrags
        depthOption = DepthOption Less True

    drawDepth (\env -> (NoBlending, deferredShaderDepthImage env, depthOption)) geometryFragsWithDepth $ \(p, n ,c) -> do
        drawColor (\env -> (deferredShaderPositionImage env, pure True, False)) p
        drawColor (\env -> (deferredShaderNormalImage env, pure True, False)) n
        drawColor (\env -> (deferredShaderMaterialImage env, pure True, False)) c

    drawWindowDepth (const (window, depthOption)) (snd <$> geometryFragsWithDepth)

data SsaoShaderEnvironment os = SsaoShaderEnvironment
    { ssaoShaderViewport :: ViewPort
    , ssaoShaderPrimitives :: PrimitiveArray Triangles (B2 Float)
    , ssaoShaderDepthImage :: Image (Format Depth)
    , ssaoShaderPositionTex :: Texture2D os (Format RGBFloat)
    , ssaoShaderNormalTex :: Texture2D os (Format RGBFloat)
    , ssaoShaderKernelTex :: Texture1D os (Format RGBFloat)
    , ssaoShaderNoiseTex :: Texture2D os (Format RGBFloat)
    }

ssaoShader :: Window os RGBAFloat Depth -> Buffer os (Uniform ShaderConfigB) -> Shader os (SsaoShaderEnvironment os) ()
ssaoShader _ shaderConfigUniformBuffer = do
    config <- getUniform (const (shaderConfigUniformBuffer, 0))
    let viewProjMat = shaderConfigProjectionS config
        cameraMat = shaderConfigCameraS config !*! shaderConfigTransformationS config

    triangles :: PrimitiveStream Triangles (V4 VFloat, ()) <- do
        coords <- toPrimitiveStream ssaoShaderPrimitives
        return $ (\(V2 x y) -> (V4 x y 0 1, ())) <$> coords

    let getRasterOptions env = (Front, ssaoShaderViewport env, DepthRange 0 1)

    positionSampler <- newSampler2D $ \env ->
        (ssaoShaderPositionTex env, SamplerFilter Nearest Nearest Nearest Nothing, (pure ClampToEdge, 0))
    normalSampler <- newSampler2D $ \env ->
        (ssaoShaderNormalTex env, SamplerFilter Nearest Nearest Nearest Nothing, (pure ClampToEdge, 0))
    kernelSampler <- newSampler1D $ \env ->
        (ssaoShaderKernelTex env, SamplerFilter Nearest Nearest Nearest Nothing, (ClampToEdge, 0))
    noiseSampler <- newSampler2D $ \env ->
        (ssaoShaderNoiseTex env, SamplerFilter Nearest Nearest Nearest Nothing, (pure Repeat, 0))

    frags :: FragmentStream () <- rasterize getRasterOptions triangles
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

            position = cameraMat !* point (positionSample texCoords)
            normal = (cameraMat !* vector (normalSample texCoords))^._xyz

            tangent = signorm (noise - normal ^* dot noise normal)
            bitangent = cross normal tangent
            tbn = transpose $ V3 tangent bitangent normal

            kernelTexSize = sampler1DSize kernelSampler 0

            contributions = snd $ while ((0 <*) . fst) getContribution (kernelTexSize, 0)

            getContribution (i, acc) = (i - 1, acc + contribution) where
                sample = kernelSample (toFloat i / toFloat kernelTexSize)
                radius = 2
                bias = 0.01

                -- From tangent to view-space
                samplePosition = position + vector ((tbn !* sample) ^* radius)

                -- From view to clip-space + perspective divide + to range [0, 1]
                texCoords' = (normalizePoint (viewProjMat !* samplePosition) * 0.5 + 0.5)^._xy

                position' = cameraMat !* point (positionSample texCoords')
                rangeCheck = smoothstep 0 1 (radius / abs (position^._z - position'^._z))
                contribution = (ifThenElse' (position'^._z >=* samplePosition^._z + bias) 1 0) * rangeCheck

            occlusion = 1 - contributions / toFloat kernelTexSize

    let
        fragsWithDepth = withRasterizedInfo
            (\a _ -> (a, a)) occlusionFrags
        depthOption = DepthOption Always True

    drawDepth (\env -> (NoBlending, ssaoShaderDepthImage env, depthOption)) fragsWithDepth
        (const $ return ())

generateSampleKernel :: Int -> IO [V3 Float]
generateSampleKernel size = forM [0 .. size-1] $ \i -> do
    [x, y, z] <- replicateM 3 (runRandomIO $ getRandomR (0, 1))
    let s = fromIntegral i / fromIntegral size
        sample = lerp (s * s) 0.1 1 * normalize (V3 (x * 2 - 1) (y * 2 - 1) z)
    return sample

generateSampleKernelTexture :: MonadIO m => Int -> ContextT GLFW.Handle os m (Texture1D os (Format RGBFloat))
generateSampleKernelTexture size = do
    sampleKernel <- liftIO (generateSampleKernel size)
    texture <- newTexture1D' "kernel" RGB16F size maxBound
    writeTexture1D texture 0 0 size sampleKernel
    return texture

data BlurShaderEnvironment os = BlurShaderEnvironment
    { blurViewport :: ViewPort
    , blurPrimitives :: PrimitiveArray Triangles (B2 Float)
    , blurDepthImage :: Image (Format Depth)
    , blurDepthTex :: Texture2D os (Format Depth)
    }

blurShader :: Window os RGBAFloat Depth -> Shader os (BlurShaderEnvironment os) ()
blurShader _ = do
    triangles :: PrimitiveStream Triangles (V4 VFloat, ()) <- do
        texCoords <- toPrimitiveStream blurPrimitives
        return $ (\(V2 x y) -> (V4 x y 0 1, ())) <$> texCoords

    depthSampler <- newSampler2D $ \env ->
        ( blurDepthTex env
        , SamplerFilter Nearest Nearest Nearest Nothing
        , (pure ClampToEdge, 0)
        )

    let texelSize@(V2 w h) = toFloat <$> sampler2DSize depthSampler 0
        getRasterOptions env = (Front, blurViewport env, DepthRange 0 1)

    frags :: FragmentStream () <- rasterize getRasterOptions triangles
    let
        depthSample = sample2D depthSampler SampleAuto Nothing Nothing
        blurredFrags = withRasterizedInfo blur frags
        blur c f =
            let (V4 x y _ _) = rasterizedFragCoord f
                texCoords = V2 (x / w) (y / h)
                r = 2 :: Int
                offsets = [(fromIntegral <$> V2 dx dy) / texelSize | dx <- [-r .. r], dy <- [-r .. r]]
                offsetCount = fromIntegral $ (2*r+1) * (2*r+1)
                averageDepth = sum (map (\offset -> depthSample (texCoords + offset)) offsets) / offsetCount
            in  (c, averageDepth)

        depthOption = DepthOption Always True

    drawDepth (\env -> (NoBlending, blurDepthImage env, depthOption)) blurredFrags (const (return ()))

data LightingShaderEnvironment os = LightingShaderEnvironment
    { lightingShaderViewport :: ViewPort
    , lightingShaderPrimitives :: PrimitiveArray Triangles (B2 Float)
    , lightingShaderShadowTex :: Texture2D os (Format Depth)
    , lightingScreenPositionTex :: Texture2D os (Format RGBFloat)
    , lightingScreenNormalTex :: Texture2D os (Format RGBFloat)
    , lightingScreenMaterialTex :: Texture2D os (Format RGBAFloat)
    , lightingShaderOcclusionTex :: Texture2D os (Format Depth)
    }

lightingShader :: Window os RGBAFloat Depth -> Buffer os (Uniform ShaderConfigB) -> Shader os (LightingShaderEnvironment os) ()
lightingShader window shaderConfigUniformBuffer = do
    config <- getUniform (const (shaderConfigUniformBuffer, 0))
    let V2 dx dy = shaderConfigViewPortOffsetS config
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

    let
        getRasterOptions env = (Front, lightingShaderViewport env, DepthRange 0 1)

        createSampler2D getTexture = newSampler2D $ \env ->
            ( getTexture env
            , SamplerFilter Nearest Nearest Nearest Nothing
            , (pure ClampToEdge, 0)
            )

    shadowSampler <- createSampler2D lightingShaderShadowTex
    positionSampler <- createSampler2D lightingScreenPositionTex
    normalSampler <- createSampler2D lightingScreenNormalTex
    materialSampler <- createSampler2D lightingScreenMaterialTex
    occlusionSampler <- createSampler2D lightingShaderOcclusionTex

    frags :: FragmentStream () <- rasterize getRasterOptions triangles
    let
        shadowSample = sample2D shadowSampler SampleAuto Nothing Nothing
        positionSample = sample2D positionSampler SampleAuto Nothing Nothing
        normalSample = sample2D normalSampler SampleAuto Nothing Nothing
        materialSample = sample2D materialSampler SampleAuto Nothing Nothing
        occlusionSample = sample2D occlusionSampler SampleAuto Nothing Nothing

        V2 w h = toFloat <$> sampler2DSize positionSampler 0

        litFrags = flip withRasterizedInfo frags $ \_ f ->
            let (V4 x y _ _) = rasterizedFragCoord f
                texCoords = V2 ((x - dx) / w) ((y - dy) / h)
                position = positionSample texCoords
                normal = normalSample texCoords
                material = materialSample texCoords
                occlusion = occlusionSample texCoords
                lighting = getSunlight
                    shadowSample
                    (normMat !* normal)
                    (Just $ shadowMat !* point position)
                    position
                    material
                    occlusion
                    lightingContext
            in  lighting

        colorOption = ContextColorOption NoBlending (pure True)

    drawWindowColor (const (window, colorOption)) litFrags

data GridShaderEnvironment = GridShaderEnvironment
    { gridViewport :: ViewPort
    , gridPrimitives :: PrimitiveArray Triangles (B3 Float)
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
            point .
            (* 4000) <$> triangles

        getRasterOptions env = (Front, gridViewport env, DepthRange 0 1)

    fragCoord :: FragmentStream (V2 FFloat, FFloat, FogS F) <- rasterize getRasterOptions projectedTriangles
    let
        drawGridLine :: (V2 FFloat, FFloat, FogS F) -> V4 FFloat
        drawGridLine (p, camPosZ, fog) = color' where
            -- Pick a coordinate to visualize in a grid
            coord = p / 40
            -- Compute anti-aliased renderContext-space grid lines
            V2 gx gy = abs (fract' (coord - 0.5) - 0.5) / (fwidth <$> coord)
            line = minB gx gy
            -- Just visualize the grid lines directly
            color = V4 1 0 0 (1 - minB line 1)
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

data ScreenShaderEnvironment os = ScreenShaderEnvironment
    { screenViewport :: ViewPort
    , screenPrimitives :: PrimitiveArray Triangles (B2 Float)
    , screenDepthTex :: Texture2D os (Format Depth)
    }

screenShader :: Window os RGBAFloat Depth -> Shader os (ScreenShaderEnvironment os) ()
screenShader window = do
    triangles :: PrimitiveStream Triangles (V4 VFloat, ()) <- do
        texCoords <- toPrimitiveStream screenPrimitives
        return $ (\(V2 x y) -> (V4 x y 0 1, ())) <$> texCoords

    depthSampler <- newSampler2D $ \env ->
        ( screenDepthTex env
        , SamplerFilter Nearest Nearest Nearest Nothing
        , (pure ClampToEdge, 0)
        )

    let getRasterOptions env = (Front, screenViewport env, DepthRange 0 1)

    frags :: FragmentStream () <- rasterize getRasterOptions triangles
    let
        depthSample = sample2D depthSampler SampleAuto Nothing Nothing
        colorFrags = withRasterizedInfo (\_ f -> pickDepthColor f) frags
        V2 w h = toFloat <$> sampler2DSize depthSampler 0
        pickDepthColor f =
            let (V4 x y _ _) = rasterizedFragCoord f
                texCoords = V2 (x / w) (y / h)
                d = depthSample texCoords
            in  V4 d d d 1

        colorOption = ContextColorOption NoBlending (pure True)

    drawWindowColor (const (window, colorOption)) colorFrags

data NormalShaderEnvironment os = NormalShaderEnvironment
    { normalShaderViewport :: ViewPort
    , normalShaderPrimitives :: PrimitiveArray Lines (B3 Float)
    }

normalShader :: Window os RGBAFloat Depth -> Buffer os (Uniform ShaderConfigB) -> Shader os (NormalShaderEnvironment os) ()
normalShader window shaderConfigUniformBuffer = do
    config <- getUniform (const (shaderConfigUniformBuffer, 0))
    let viewProjMat = shaderConfigProjectionS config !*! shaderConfigCameraS config !*! shaderConfigTransformationS config

    lines :: PrimitiveStream Lines (V3 VFloat) <- toPrimitiveStream normalShaderPrimitives
    let
        projectedLines :: PrimitiveStream Lines (V4 VFloat, ())
        projectedLines = (\p -> (viewProjMat !* point p, ())) <$> lines

        getRasterOptions env = (Front, normalShaderViewport env, DepthRange 0 1)

    frags :: FragmentStream () <- rasterize getRasterOptions projectedLines
    let
        coloredFragsWithDepth = withRasterizedInfo
            (\_ p -> (V4 1 1 0 1, rasterizedFragCoord p ^. _z)) frags
        colorOption = ContextColorOption NoBlending (pure True)
        depthOption = DepthOption Less True

    drawWindowColorDepth (const (window, colorOption, depthOption)) coloredFragsWithDepth

------------------------------------------------------------------------------------------------------------------------

type LightingContext x =
    ( V3 (S x Float) -- camera position
    , FogS x
    , DirectionLightS x -- sun
    -- move to material
    , S x Float -- material specular intensity
    , S x Float -- material specular power
    )

getSunlight :: (V2 (S x Float) -> ColorSample x Depth)
    -> V3 (S x Float)
    -> Maybe (V4 (S x Float))
    -> V3 (S x Float)
    -> V4 (S x Float)
    -> S x Float
    -> LightingContext x
    -> V4 (S x Float)
getSunlight shadowSample normal shadowCoord renderContextSpacePosition material occlusion lightingContext =
    let (camPos, fog, sun, specularIntensity, specularPower) = lightingContext

        DirectionLightS sunLightColor sunLightDirection sunLightAmbientIntensity = sun
        cameraDirection = signorm (camPos - renderContextSpacePosition)
        baseColor = material^._xyz

        ambient = sunLightColor ^* sunLightAmbientIntensity
        diffuse = sunLightColor ^* maxB 0 (dot normal (-sunLightDirection))
        specular = getSpecularColor cameraDirection normal specularIntensity specularPower (sunLightColor * 1.5) (-sunLightDirection)
        shadow = maybe 1 (getShadow shadowSample normal (-sunLightDirection)) shadowCoord
        sunContribution = (baseColor * ambient) + (baseColor * diffuse + specular) ^* shadow

        color = point (sunContribution ^* occlusion)

        -- Add fog.
        fogDistance = norm $ camPos - renderContextSpacePosition
        color' = applyFog fog color fogDistance

    in  ifThenElse' (material^._w <* 1) (V4 0.5 0.5 0.5 1) color'

getLight :: (V2 FFloat -> ColorSample F Depth)
    -> V3 FFloat
    -> V3 FFloat
    -> V4 FFloat
    -> FFloat
    -> LightingContext F
    -> PointLightS F
    -> V4 FFloat
getLight shadowSample normal renderContextSpacePosition material occlusion lightingContext light =
    let (camPos, _, _, specularIntensity, specularPower) = lightingContext

        PointLightS lightPosition lightColor = light
        cameraDirection = signorm (camPos - renderContextSpacePosition)
        baseColor = material^._xyz

        lightDirection = signorm (lightPosition - renderContextSpacePosition)
        lightDistance = norm (lightPosition - renderContextSpacePosition)
        attenuation = 0.01 + 0.07 * lightDistance + 0.00008 * lightDistance * lightDistance

        diffuse = lightColor ^* maxB 0 (dot normal lightDirection)
        specular = getSpecularColor cameraDirection normal specularIntensity specularPower lightColor lightDirection

        lightContribution = (baseColor * diffuse + specular) ^/ attenuation

        color = point (lightContribution ^* occlusion)

    in  color

getFogFactor :: FogS x -> S x Float -> S x Float
getFogFactor fog fogDistance =
    let fogEquation = FogExp2
        factor = case fogEquation of
            FogLinear -> (fogEndS fog - fogDistance) / (fogEndS fog - fogStartS fog)
            FogExp -> exp (-fogDensityS fog * fogDistance)
            FogExp2 -> exp (-(fogDensityS fog * fogDistance)^^2)
    in  1 - clamp factor 0 1

applyFog :: FogS x -> V4 (S x Float) -> S x Float -> V4 (S x Float)
applyFog fog color fogDistance = mix color (fogColorS fog) (V4 a a a 0) where a = getFogFactor fog fogDistance

getSpecularColor :: V3 (S x Float)
    -> V3 (S x Float)
    -> S x Float
    -> S x Float
    -> V3 (S x Float)
    -> V3 (S x Float)
    -> V3 (S x Float)
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
reflect :: V3 (S x Float) -- ^ Specifies the incident vector.
    -> V3 (S x Float) -- ^ Specifies the normal vector.
    -> V3 (S x Float)
reflect i n = i - 2 ^* dot n i * n

getShadow :: (V2 (S x Float) -> ColorSample x Depth) -> V3 (S x Float) ->  V3 (S x Float) -> V4 (S x Float) -> S x Float
getShadow shadowSample normal sunLightDirection (V4 x y z w) = maxB 0 visibility where
    rectify c = (c/w + 1) / 2
    texCoords = V2 (rectify x) (rectify y)

    cosTheta = clamp (dot normal sunLightDirection) 0 1
    bias = clamp (0.005 * tan (acos cosTheta)) 0 0.01

    sample offset = shadowSample (texCoords + offset / 700)
    getContribution zShadow = ifThenElse' (zShadow <* rectify z - bias) 1 0

    visibility = 1 - 0.75 * sum (getContribution . sample <$> poissonDisk) / fromIntegral (length poissonDisk)

poissonDisk :: [V2 (S x Float)]
poissonDisk =
   [ V2 (-0.94201624) (-0.39906216)
   , V2 0.94558609 (-0.76890725)
   , V2 (-0.094184101) (-0.92938870)
   , V2 0.34495938 0.29387760
   , V2 (-0.91588581) 0.45771432
   , V2 (-0.81544232) (-0.87912464)
   , V2 (-0.38277543) 0.27676845
   , V2 0.97484398 0.75648379
   , V2 0.44323325 (-0.97511554)
   , V2 0.53742981 (-0.47373420)
   , V2 (-0.26496911) (-0.41893023)
   , V2 0.79197514 0.1909018
   , V2 (-0.24188840) 0.99706507
   , V2 (-0.81409955) 0.91437590
   , V2 0.19984126 0.78641367
   , V2 0.14383161 (-0.14100790)
   ]
