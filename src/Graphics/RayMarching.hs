{-# language ScopedTypeVariables, TypeFamilies, FlexibleContexts, AllowAmbiguousTypes #-}

module Graphics.RayMarching
    ( createRayMarchingRenderer
    )
where

import Prelude hiding ((.), id, (<*))
import Control.Category (Category((.)), id)
import Control.Lens ((&), (<&>), (^.), index, un)
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
sdSphere :: (Metric f, Floating a) => f a -> a -> a
sdSphere p s = norm p - s

-- Box - exact (Youtube Tutorial with derivation: https://www.youtube.com/watch?v=62-pRVZuS5c)
sdBox :: (IfB a, OrdB a, Metric f, Floating a, Foldable f, Floating (f a)) => f a -> f a -> a
sdBox p b =
    let q = abs p - b
    in  norm (maxB 0 <$> q) + minB 0 (foldl1 maxB q)

-- Box Frame - exact (https://www.shadertoy.com/view/3ljcRh)
sdBoxFrame :: (IfB a, OrdB a, Floating a) => V3 a -> V3 a -> V3 a -> a
sdBoxFrame p b e =
    let p@(V3 px py pz) = abs p - b
        q@(V3 qx qy qz) = abs (p + e) - e
        f x = norm (maxB 0 <$> x) + minB 0 (foldl1 maxB x)
    in  minimumB
            [ f (V3 px qy qz)
            , f (V3 qx py qz)
            , f (V3 qx qy pz)
            ]

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
sceneDistance' p =
    let p' = mod'' p (pure 40)
        sphere center radius = norm (p' - center) - radius
        box center size = let V3 x y z = (abs (p' - center) - size) in maximumB [x, y, z]
    in  minimumByB (comparingB fst)
            [ (sphere (V3 10 10 10) 5, red)
            , (sphere (V3 20 10 10) 6, blue)
            , (box (V3 10 14 10) (V3 2 4 6), green)
            ]

sceneDistance :: V3 FFloat -> FFloat
sceneDistance = fst . sceneDistance'

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

onTexel :: (V2 FFloat, V3 FFloat, FFloat, FFloat, V4 (V4 FFloat)) -> RasterizedInfo -> V4 FFloat
onTexel (V2 x y, eye, near, far, invModelViewProj) _ = point color' where
    nearRay = rectify (invModelViewProj !* V4 x y (-1) 1) - eye
    rectify (V4 x y z w) = V3 (x / w) (y / w) (z / w)
    slantNearDistance = norm nearRay
    slantFarDistance = far * slantNearDistance / near
    ray = nearRay ^/ slantNearDistance
    (rayMarch, color) = fst $ while
        (\((d, c), s) -> d <* slantFarDistance &&* s >* 1e-2)
        (\((d, c), s) -> let (s', c') = sceneDistance' (eye + ray ^* (d + s)) in ((d + s, c'), s'))
        ((0, black), slantNearDistance)
    color' = ifThenElse' (rayMarch <* slantFarDistance) (color ^* f) lightGrey
    n = fastNormal (eye + ray ^* rayMarch)
    f = dot (V3 0 0 1) n

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
        (eye, near, far, invModelViewProj) <- getUniform (const (cameraBuffer, 0))

        triangles :: PrimitiveStream Triangles (VPos, (V2 VFloat, V3 VFloat, VFloat, VFloat, V4 (V4 VFloat))) <- toPrimitiveStream snd
            <&> fmap (\(V2 x y) -> (V4 x y 0 1, (V2 x y, eye, near, far, invModelViewProj)))

        let rasterOptions = \(viewPort, _) -> (Front, viewPort, DepthRange 0 1)
        fs :: FragmentStream (V4 FFloat) <- rasterize rasterOptions triangles
            <&> withRasterizedInfo onTexel

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
