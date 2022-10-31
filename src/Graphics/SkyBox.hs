{-# language ScopedTypeVariables #-}

module Graphics.SkyBox
    ( createSkyBoxRenderer
    )
where

import Control.Lens ((&), (<&>), (^.), index)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Exception
import Graphics.GPipe

import Common.Debug

import Graphics.MarchingCube
import Graphics.Texture

createSkyBoxRenderer :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m)
    => Window os RGBAFloat Depth
    -> Buffer os (Uniform (V4 (B4 Float), V4 (B4 Float), B3 Float))
    -> ContextT ctx os m (ViewPort -> Render os ())
createSkyBoxRenderer window projectionBuffer = do
    let cube =
            [ V3 (-1) (-1) (-1)
            , V3 1 (-1) (-1)
            , V3 (-1) 1 (-1)
            , V3 1 1 (-1)
            , V3 (-1) (-1) 1
            , V3 1 (-1) 1
            , V3 (-1) 1 1
            , V3 1 1 1
            ]
        cubeFaces =
            [ (0, 1, 3, 2) -- dn
            , (5, 7, 3, 1) -- ft
            , (7, 6, 2, 3) -- rt
            , (6, 4, 0, 2) -- bk
            , (4, 5, 1, 0) -- lf
            , (4, 6, 7, 5) -- up
            ]

    let vertices = flip concatMap (zip [0..] cubeFaces) $ \(n, (i, j, k, l)) ->
            let [a, b, c, d] = map (cube !!) [i, j, k, l]
            in  [a, b, c, c, d, a]

    boxBuffer :: Buffer os (B3 Float) <- newBuffer (length vertices)
    writeBuffer boxBuffer 0 vertices

    Just skyTexture <- loadCubeImage 512 $ -- [CubeNegX, CubePosX, CubeNegY, CubePosY, CubeNegZ, CubePosZ]
        map (\ext -> "data/cloudtop/cloudtop_" ++ ext ++ ".tga") ["lf", "rt", "bk", "ft", "dn", "up"]

    shader :: CompiledShader os (ViewPort, PrimitiveArray Triangles (B3 Float))  <- compileShader $ do
        (projectionMat, cameraMat, cameraPos) <- getUniform (const (projectionBuffer, 0))
        let modelViewProj = projectionMat !*! cameraMat

        skySampler <- newSamplerCube (const (skyTexture, SamplerFilter Linear Linear Linear (Just 4)))

        let project p = (modelViewProj !* point (p * 500 + cameraPos), p)
        triangles :: PrimitiveStream Triangles (V4 VFloat, V3 VFloat) <- fmap project <$> toPrimitiveStream snd

        let rasterOptions = \(viewPort, _) -> (Front, viewPort, DepthRange 0 1)
        fs :: FragmentStream (V4 FFloat) <- rasterize rasterOptions triangles
            <&> withRasterizedInfo (\stf p -> point $ sampleCube skySampler (SampleLod 0) stf)

        let colorOption = ContextColorOption NoBlending (pure True)
        drawWindowColor (const (window, colorOption)) fs

    return $ \viewPort -> do
        vertexArray <- toPrimitiveArray TriangleList
            <$> newVertexArray boxBuffer
        shader (viewPort, vertexArray)
