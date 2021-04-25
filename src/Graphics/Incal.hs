{-# LANGUAGE FlexibleInstances, UndecidableInstances, ScopedTypeVariables, TypeFamilies, FlexibleContexts #-}

module Graphics.Incal
    ( createIncalRenderer
    ) where

import Prelude hiding ((<*))

import Control.Applicative (pure)
import Graphics.Color
import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW

import Graphics.Geometry
import Graphics.Texture
import Graphics.Shaders

createIncalRenderer :: Window os RGBAFloat Depth -> ContextT GLFW.Handle os IO (RenderContext os)
createIncalRenderer window = do

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
    compiledLightingShader <- compileShader $ lightingShader window shaderConfigUniformBuffer
    compiledGridShader <- compileShader $ gridShader window shaderConfigUniformBuffer
    compiledScreenShader <- compileShader . silenceShader $ screenShader window
    compiledNormalShader <- compileShader $ normalShader window shaderConfigUniformBuffer

    let renderAction context bounds camera _ sun lights buffers normalBuffers = do
            let (_, (w, h)) = bounds
                size = V2 w h
            frameBufferGroup <- case renderContextFrameBufferGroup context of
                Just frameBufferGroup -> if fst frameBufferGroup == size
                    then return frameBufferGroup
                    else createFrameBufferGroup size
                Nothing -> createFrameBufferGroup size

            shadowMat <- renderShadow bounds (snd frameBufferGroup) camera sun buffers
            renderObjects shadowMat bounds (snd frameBufferGroup) camera sun lights buffers normalBuffers

            return $ RenderContext (Just frameBufferGroup) renderAction

        fog = Fog (V4 0.5 0.5 0.5 1) 10 100 0.2

        renderShadow bounds@((x, y), _) frameBufferGroup camera sun buffers = do
            let
                r = cameraNear camera / 25 -- 50
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
                (fromIntegral <$> V2 x y)
                (cameraPosition camera)
                projectionMat
                cameraMat
                identity -- Transformation
                1 -- ShadowUsed
                shadowMat
                fog
                sun
                0 -- TimePassed
                ]

            writeBuffer pointLightUniformBuffer 0 []

            render $ do
                let shadowTex = frameBufferGroupShadowTex frameBufferGroup
                    shadowTexSize = head (texture2DSizes shadowTex)
                sImage <- getTexture2DImage shadowTex 0
                clearImageDepth sImage 1
                primArray <- mconcat <$> mapM (fmap (toPrimitiveArray TriangleList) . newVertexArray) buffers
                compiledShadowShader $ ShadowShaderEnvironment primArray sImage

            return shadowMat

        renderObjects shadowMat bounds frameBufferGroup camera sun lights buffers normalBuffers = do
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
                projectionMat = perspective (cameraFov camera) (fromIntegral w / fromIntegral h) (cameraNear camera) (cameraFar camera)
                -- Eye, Center, Up
                cameraMat = lookAt
                    (cameraPosition camera)
                    (cameraPosition camera + getSight camera)
                    (getUp camera)

            writeBuffer shaderConfigUniformBuffer 0 [ShaderConfig
                (fromIntegral <$> V2 x y)
                (cameraPosition camera)
                projectionMat
                cameraMat
                identity -- Transformation
                0 -- ShadowUsed
                shadowMat
                fog
                sun
                0 -- TimePassed
                ]

            writeBuffer pointLightUniformBuffer 0 lights

            let viewPort = ViewPort (V2 x y) (V2 w h)
                viewPort' = ViewPort (V2 0 0) (V2 w h)

            {- I don’t know if having multiple calls to 'render' is a workaround
            to some bugs (see https://github.com/tobbebex/GPipe-Core/issues/50)
            or something with a well defined behavior (some error messages give
            this advice, especially when trying to use the same texture as an
            input and output).
            -}

            -- Direct rendering
            render $ do
                clearWindowDepth window 1
                primArray <- mconcat <$> mapM (fmap (toPrimitiveArray TriangleList) . newVertexArray) buffers
                compiledDirectShader $ DirectShaderEnvironment viewPort primArray shadowTex

            -- Deferred rendering
            render $ do
                clearWindowDepth window 1
                dImage <- getTexture2DImage depthTex 0
                pImage <- getTexture2DImage positionTex 0
                nImage <- getTexture2DImage normalTex 0
                mImage <- getTexture2DImage materialTex 0
                clearImageDepth dImage 1
                clearImageColor pImage 0
                clearImageColor nImage 0
                clearImageColor mImage 0
                primArray <- mconcat <$> mapM (fmap (toPrimitiveArray TriangleList) . newVertexArray) buffers
                compiledDeferredShader $ DeferredShaderEnvironment viewPort' primArray dImage pImage nImage mImage

            -- SSAO
            render $ do
                oImage <- getTexture2DImage occlusionTex 0
                clearImageDepth oImage 1
                screenPrimArray <- toPrimitiveArray TriangleList <$> newVertexArray screenBuffer
                compiledSsaoShader $ SsaoShaderEnvironment viewPort' screenPrimArray oImage positionTex normalTex sampleKernelTex noiseTex

            -- SSAO Bluring
            render $ do
                bImage <- getTexture2DImage blurredOcclusionTex 0
                clearImageDepth bImage 1
                screenPrimArray <- toPrimitiveArray TriangleList <$> newVertexArray screenBuffer
                compiledBlurShader $ BlurShaderEnvironment viewPort' screenPrimArray bImage occlusionTex

            -- Lighting
            render $ do
                screenPrimArray <- toPrimitiveArray TriangleList <$> newVertexArray screenBuffer
                compiledLightingShader $ LightingShaderEnvironment
                    viewPort
                    screenPrimArray
                    shadowTex
                    positionTex
                    normalTex
                    materialTex
                    blurredOcclusionTex

            -- Tone mapping

            -- Gaussian blur

            -- Combine

            -- Debug
            render $ do
                screenPrimArray <- toPrimitiveArray TriangleList <$> newVertexArray screenBuffer
                compiledScreenShader $ ScreenShaderEnvironment viewPort screenPrimArray occlusionTex

            -- Debug: normals
            render $ do
                normalPrimArray <- mconcat <$> mapM (fmap (toPrimitiveArray LineStrip) . newVertexArray) normalBuffers
                compiledNormalShader $ NormalShaderEnvironment viewPort normalPrimArray

            -- Post (direct rendering)
            render $ do
                gridPrimArray <- toPrimitiveArray TriangleList <$> newVertexArray gridBuffer
                compiledGridShader $ GridShaderEnvironment viewPort gridPrimArray

    return $ RenderContext Nothing renderAction
