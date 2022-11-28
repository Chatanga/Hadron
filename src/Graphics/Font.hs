{-# language ScopedTypeVariables, TypeFamilies, FlexibleContexts #-}

module Graphics.Font (
    createBitmapTextPreRenderer,
    createTextPreRenderer,
    createDistanceFieldAtlas,
    createDistanceFieldLetter
) where

import Prelude hiding (id, (<*), (>*))

import Codec.Picture
import Control.Lens ((&), (<&>), (^.))
import Control.Monad
import Control.Monad.Exception
import Control.Monad.State
import Control.Parallel.Strategies
import Data.ByteString.Internal
import Data.Bits
import Data.Functor
import qualified Data.IntMap as IntMap
import Data.List
import Data.Maybe
import Data.Ord
import Data.Word (Word8, Word16, Word32)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import Foreign (allocaArray, alloca, peek, plusPtr, withArray, peekArray, nullPtr, Ptr)
import Foreign.C.String (withCString, CString)
import Foreign.C.Types (CChar)
import Graphics.GPipe
import Graphics.Rendering.FreeType.Internal
import Graphics.Rendering.FreeType.Internal.Bitmap
import Graphics.Rendering.FreeType.Internal.BitmapSize (FT_Bitmap_Size)
import Graphics.Rendering.FreeType.Internal.PrimitiveTypes
import Graphics.Rendering.FreeType.Internal.Library (FT_Library)
import Graphics.Rendering.FreeType.Internal.Face
import Graphics.Rendering.FreeType.Internal.GlyphSlot
import System.Log.Logger

import Common.Debug
import Graphics.Color
import Graphics.Texture

----------------------------------------------------------------------------------------------------

createBitmapTextPreRenderer :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m)
    => Window os RGBAFloat Depth
    -> Buffer os (Uniform (V4 (B4 Float), V4 (B4 Float), B3 Float))
    -> ContextT ctx os m (V3 Float -> Float -> String -> ContextT ctx os m (ViewPort -> Render os ()))
createBitmapTextPreRenderer window projectionBuffer = do

    Just atlasTexture <- loadImage "data/charmap-oldschool_white.png"

    let maxTextLength = 128
    textBuffer :: Buffer os (B Word32) <- newBuffer maxTextLength

    positionAndScaleBuffer :: Buffer os (Uniform (B3 Float, B Float)) <- newBuffer 1

    return $ \position scale text -> do

        writeBuffer textBuffer 0 (take maxTextLength (fromIntegral . (\c -> c - 32) . c2w <$> text))
        writeBuffer positionAndScaleBuffer 0 [(position, scale)]

        shader :: CompiledShader os (ViewPort, PrimitiveArray Points (B Word32))  <- compileShader $ do

            atlasSampler <- newSampler2D (const (atlasTexture, SamplerNearest, (pure ClampToEdge, undefined)))

            (projectionMat, cameraMat, cameraPos) <- getUniform (const (projectionBuffer, 0))
            let modelViewProj = projectionMat !*! cameraMat

            (position, scale) <- getUniform (const (positionAndScaleBuffer, 0))

            ps :: primitiveStream Points VWord <- toPrimitiveStream snd
            let ps' :: primitiveStream Points (VPos, VInt, VInt) = ps &
                    withInputIndices (\charCode indices -> (modelViewProj !* point position, toInt charCode, inputVertexID indices))

            gs :: GeometryStream (Geometry Points (VPos, VInt, VInt)) <- geometrize ps'
            let makeLetter :: Geometry Points (VPos, VInt, VInt) -> GGenerativeGeometry Triangles (VPos, V2 VFloat)
                makeLetter (Point (p, charCode, index)) =
                    let (s, t) = (toFloat $ mod' charCode 18, toFloat $ div' charCode 18)
                        rectify (V4 x y z w) = V4 (x / w) (y / w) (z / w) 1
                        toVertice x y =
                            ( p + scale * p^._w *^ V4 (toFloat index + x) y 0 0
                            , V2 ((128 - (s + x) * 7) / 128) ((64 - (t + 1 - y) * 9) / 64)
                            )
                    in  generativeTriangleStrip
                        & emitVertexPosition (toVertice 0 0)
                        & emitVertexPosition (toVertice 0 1)
                        & emitVertexPosition (toVertice 1 1)
                        & endPrimitive
                        & emitVertexPosition (toVertice 0 0)
                        & emitVertexPosition (toVertice 1 1)
                        & emitVertexPosition (toVertice 1 0)
                        & endPrimitive
                gs' :: GeometryStream (GGenerativeGeometry Triangles (VPos, V2 VFloat)) = makeLetter <$> gs

            let rasterOptions = \(viewPort, _) -> (FrontAndBack, viewPort, DepthRange 0 1)
                maxVertices = 6

            let withDepth = false
            if withDepth
                then do
                    fs :: FragmentStream (V4 FFloat, FragDepth) <- generateAndRasterize rasterOptions maxVertices gs'
                        <&> withRasterizedInfo (\st ri -> (let c = sample2D atlasSampler SampleAuto Nothing Nothing st in point c, rasterizedFragCoord ri ^. _z))

                    let colorOption = ContextColorOption NoBlending (pure True)
                        depthOption = DepthOption Less True
                    drawWindowColorDepth (const (window, colorOption, depthOption)) fs
                else do
                    fs :: FragmentStream (V4 FFloat) <- generateAndRasterize rasterOptions maxVertices gs'
                        <&> fmap (\st -> let c = sample2D atlasSampler SampleAuto Nothing Nothing st in point c)

                    let colorOption = ContextColorOption NoBlending (pure True)
                    drawWindowColor (const (window, colorOption)) fs

        return $ \viewPort -> do
            letters <- toPrimitiveArray PointList . takeVertices (length text) <$> newVertexArray textBuffer
            shader (viewPort, letters)

----------------------------------------------------------------------------------------------------

atlasSize :: Num a => a
atlasSize = 2048

createTextPreRenderer :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m)
    => Window os RGBAFloat Depth
    -> Buffer os (Uniform (V4 (B4 Float), V4 (B4 Float), B3 Float))
    -> ContextT ctx os m (V3 Float -> V3 Float -> V3 Float -> String -> ContextT ctx os m (ViewPort -> Render os ()))
createTextPreRenderer window projectionBuffer = do
    atlas <- liftIO getAtlas

    Just atlasTexture <- loadGreyscaleImage "data/atlas.png"
    generateTexture2DMipmap atlasTexture

    let maxTextLength = 40
    textBuffer :: Buffer os (B3 Float, B2 Float) <- newBuffer maxTextLength

    return $ \position up advance text -> do

        let charInfos = mapMaybe ((`IntMap.lookup` atlas) . fromEnum) text
            vertices = concat $ snd $ mapAccumL toLetter position charInfos

            toLetter :: V3 Float -> CharInfo -> (V3 Float, [(V3 Float, V2 Float)])
            toLetter p (CharInfo cc b e l t w r) = (p', vertices) where
                [b', e', l', t', w', r'] = map fromIntegral [b, e, l, t, w, r]
                p' = p ^+^ ((w' - l') *^ advance)
                p1 = p ^-^ (l' *^ advance) ^+^ (up ^* t')
                p2 = p1 ^+^ (w' *^ advance)
                p3 = p2 ^-^ (up ^* r')
                p4 = p1 ^-^ (up ^* r')
                triangles = [p1, p2, p3, p1, p3, p4]
                texCoords = [V2 b e, V2 (b+w) e, V2 (b+w) (e+r), V2 b e, V2 (b+w) (e+r), V2 b (e+r)]
                    & map ((V2 1 1 -) . (/ atlasSize) . fmap fromIntegral)
                vertices = zip triangles texCoords

        writeBuffer textBuffer 0 (take maxTextLength vertices)

        shader :: CompiledShader os (ViewPort, PrimitiveArray Triangles (B3 Float, B2 Float))  <- compileShader $ do

            atlasSampler <- newSampler2D (const (atlasTexture, SamplerFilter Linear Linear Linear (Just 4), (pure Repeat, undefined)))

            (projectionMat, cameraMat, cameraPos) <- getUniform (const (projectionBuffer, 0))
            let modelViewProj = projectionMat !*! cameraMat

            ps :: primitiveStream Triangles (V3 VFloat, V2 VFloat) <- toPrimitiveStream snd
            let ps' :: primitiveStream Triangles (VPos, V2 VFloat) = ps <&>
                    \(p, st) -> (modelViewProj !* point (p / 10), st)

            let rasterOptions = \(viewPort, _) -> (FrontAndBack, viewPort, DepthRange 0 1)
            fs :: FragmentStream (V4 FFloat, FragDepth) <- rasterize rasterOptions ps'
                <&> withRasterizedInfo (\st ri -> (calculateLetterTexelColor atlasSampler st ri, rasterizedFragCoord ri ^. _z))

            let blending = BlendRgbAlpha
                    (FuncAdd, FuncAdd)
                    (BlendingFactors SrcAlpha OneMinusSrcAlpha, BlendingFactors Zero Zero)
                    (point white)
                colorOption = ContextColorOption blending (pure True)
                -- colorOption = ContextColorOption NoBlending (pure True)
                depthOption = DepthOption Less True
            drawWindowColorDepth (const (window, colorOption, depthOption)) fs

        return $ \viewPort -> do
            vertices <- toPrimitiveArray TriangleList . takeVertices (length text) <$> newVertexArray textBuffer
            shader (viewPort, vertices)

calculateLetterTexelColor :: Sampler2D (Format RFloat) -> V2 FFloat -> RasterizedInfo -> V4 FFloat
calculateLetterTexelColor atlasSampler st ri = baseColor3 where
    baseColor0 = V4 0 0 0 1

    -- size = sampler2DSize atlasSampler 0
    e = rasterizedFragCoord ri ^. _z / 10 -- Crude

    distAlphaMask = sample2D atlasSampler SampleAuto Nothing Nothing st

    outline = true :: FBool
    outlineColor = pure 1
    outlineMinValue0 = 0.40
    outlineMinValue1 = 0.45
    outlineMaxValue0 = 0.50
    outlineMaxValue1 = 0.55

    baseColor1 = ifThenElse' (outline &&* (distAlphaMask >=* outlineMinValue0) &&* (distAlphaMask <=* outlineMaxValue1))
        (
            let oFactor = ifThenElse' (distAlphaMask <=* outlineMinValue1)
                    (smoothstep outlineMinValue0 outlineMinValue1 distAlphaMask)
                    (smoothstep outlineMaxValue1 outlineMaxValue0 distAlphaMask)
            in  mix baseColor0 outlineColor (pure oFactor)
        )
        baseColor0

    softEdge = true :: FBool
    softEdgeMin = 0.50 - e
    softEdgeMax = 0.50 + e

    alpha = ifThenElse' softEdge
        (baseColor1^._w * smoothstep softEdgeMin softEdgeMax distAlphaMask)
        (ifThenElse' (distAlphaMask >=* 0.5) 1 0)
    baseColor2 = V4 (baseColor1^._x) (baseColor1^._y) (baseColor1^._z) alpha

    outerGlow = false :: FBool
    glowUvOffset = V2 0.0008 0.0008
    outerGlowColor = V4 0 0 0 0.75
    outerGlowMinValue = 0.49
    outerGlowMaxValue = 0.51
    maskUsed = 0.5

    baseColor3 = ifThenElse' outerGlow
        (
            let glowTexelMask = sample2D atlasSampler SampleAuto Nothing Nothing (st + glowUvOffset)
                glowc = outerGlowColor
                r = minB (1 - baseColor2^._w) (smoothstep outerGlowMinValue outerGlowMaxValue glowTexelMask)
            in  glowc ^* r + baseColor2 ^* (1 - r)
        )
        baseColor2

----------------------------------------------------------------------------------------------------

assertEquals :: (Show a, Eq a) => a -> a -> IO ()
assertEquals x y = unless (x == y) $ fail $ "Expected " ++ show x ++ " but got " ++ show y

runFreeType :: IO FT_Error -> IO ()
runFreeType m = do
    r <- m
    unless (r == 0) $ fail $ "FreeType Error: " ++ show r

ftInitFreeType :: IO FT_Library
ftInitFreeType = alloca $ \ptr -> do
    runFreeType $ ft_Init_FreeType ptr
    peek ptr

ftLibraryVersion :: FT_Library -> IO (FT_Int, FT_Int, FT_Int)
ftLibraryVersion ft = alloca $ \xPtr -> alloca $ \yPtr -> alloca $ \zPtr -> do
    ft_Library_Version ft xPtr yPtr zPtr
    [x, y, z] <- mapM peek [xPtr, yPtr, zPtr]
    return (x, y, z)

ftNewFace :: FT_Library -> FilePath -> IO FT_Face
ftNewFace ft fp = withCString fp $ \str ->
    alloca $ \ptr -> do
        runFreeType $ ft_New_Face ft str 0 ptr
        peek ptr

type Point = (Int, Int)

getAtlas :: IO (IntMap.IntMap CharInfo)
getAtlas = do
    entries <- map (map read . words) . lines <$> readFile "data/atlas.txt"
    return $ IntMap.fromList $ flip map entries $ \[cc, b, e, l, t, w, r] -> (cc, CharInfo cc b e l t w r)

-- FreeType (http://freetype.org/freetype2/docs/tutorial/step1.html)
createDistanceFieldAtlas :: FilePath -> IO ()
createDistanceFieldAtlas path = do
    ft <- ftInitFreeType
    (x, y, z) <- ftLibraryVersion ft
    infoM "Hadron" ("FT library version: " ++ show x ++ "." ++ show y ++ "." ++ show z)

    face <- ftNewFace ft path
    runFreeType $ ft_Set_Pixel_Sizes face 512 0

    let scale = 8

        ascii = [0x20..0xff]
        hiragana = [0x3040..0x309f]
        katakana = [0x30a0..0x30ff]

    charData <- forM (ascii ++ hiragana ++ katakana) (createDistanceFieldFromCharCode face)
    charData <- forM ascii (createDistanceFieldFromCharCode face)

    let (atlasCharData, atlasChanges) = unzip . snd $ mapAccumL insertCharImage ((0, 0), 0) charData

        scaleSize (w, h) = ((w + scale - 1) `div` scale, (h + scale - 1) `div` scale)

        insertCharImage :: (Point, Int) -> (CharInfo, ImageSource) -> ((Point, Int), (CharInfo, [(Int, Int)]))
        insertCharImage (offset@(xOffset, yOffset), hMax) (charInfo, source)
            | xOffset + w' <= atlasSize = (((xOffset + w', yOffset), max hMax h'), newCharInfo offset)
            | yOffset + h' <= atlasSize = (((w', yOffset + hMax), h'), newCharInfo (0, yOffset + hMax))
            | otherwise = error "No more place on the atlas texture"
            where
                (w', h') = scaleSize (isWitdh source, isRow source)
                (l, t) = (ciLeft charInfo `div` scale, ciTop charInfo `div` scale)
                newCharInfo (dx, dy) = (CharInfo (ciCharCode charInfo) dx dy l t w' h', copyImage (dx, dy) source)

        copyImage :: (Int, Int) -> ImageSource -> [(Int, Int)]
        copyImage offset imageSource@(ImageSource w h readCharPixel) = changes where
            (w', h') = scaleSize (w, h)
            pixels = [(x, y) | x <- [0 .. w' - 1], y <- [0 .. h' - 1]]
            changes = map (\(x, y) -> (toIndex offset x y, average imageSource x y)) pixels

        toIndex (xOffset, yOffset) x y = xOffset + x + (yOffset + y) * atlasSize

        average (ImageSource w h readCharPixel) x y = 255 - (sum values `div` length values) where
            values =
                [   uncurry readCharPixel (x', y')
                |   x' <- [x * scale .. min (w - 1) (x * scale + scale - 1)]
                ,   y' <- [y * scale .. min (h - 1) (y * scale + scale - 1)]
                ]

        -- TODO Way too slow...
        atlas = UV.replicate (atlasSize * atlasSize) 0 UV.// concat (atlasChanges `using` parList rdeepseq)

        readAtlasPixel x y = fromIntegral $ atlas UV.! (x + y * atlasSize)

    writeFile "atlas.txt" $ intercalate "\n" $ flip map atlasCharData $
        \(CharInfo cc b e l t w r) -> unwords (map show [cc, b, e, l, t, w, r])

    infoM "Hadron" "Generating atlas texture..."
    savePngImage "atlas.png" (ImageY8 (generateImage readAtlasPixel atlasSize atlasSize))
    infoM "Hadron" "Atlas texture generated"

    -- Discard the face object, as well as all of its child slots and sizes.
    runFreeType $ ft_Done_FreeType ft

createDistanceFieldLetter :: FilePath -> Char -> IO ()
createDistanceFieldLetter path char = do
    ft <- ftInitFreeType
    (x, y, z) <- ftLibraryVersion ft
    infoM "Hadron" ("FT library version: " ++ show x ++ "." ++ show y ++ "." ++ show z)

    face <- ftNewFace ft path
    runFreeType $ ft_Set_Pixel_Sizes face 512 0

    (ImageSource w r readPixel) <- snd <$> createDistanceFieldFromCharCode face (fromEnum char)

    let readPixel' x y = fromIntegral . min 255 . (* 8) . round . sqrt . fromIntegral $ readPixel x y
        finalImage = generateImage readPixel' w r :: Codec.Picture.Image Pixel8

    savePngImage ("letter-" ++ char : ".png") (ImageY8 finalImage)

    -- Discard the face object, as well as all of its child slots and sizes.
    runFreeType $ ft_Done_FreeType ft

data CharInfo = CharInfo
    {   ciCharCode :: Int
    ,   ciBegin :: Int
    ,   ciEnd :: Int
    ,   ciLeft :: Int
    ,   ciTop :: Int
    ,   ciWidth :: Int
    ,   ciRow :: Int
    } deriving Show

createDistanceFieldFromCharacter :: FT_Face -> Char -> IO (CharInfo, ImageSource)
createDistanceFieldFromCharacter face character = createDistanceFieldFromCharCode face (fromEnum character)

createDistanceFieldFromCharCode :: FT_Face -> Int -> IO (CharInfo, ImageSource)
createDistanceFieldFromCharCode face charCode = do

    -- retrieve glyph index from (Unicode) character code
    glyphIndex <- ft_Get_Char_Index face $ fromIntegral charCode

    -- load glyph image into the slot (erase previous one)
    runFreeType $ ft_Load_Char face (fromIntegral charCode) ft_LOAD_DEFAULT

    slot <- peek $ glyph face :: IO FT_GlyphSlot

    -- convert to 2-bit bitmap
    runFreeType $ ft_Render_Glyph slot ft_RENDER_MODE_MONO

    assertEquals ft_GLYPH_FORMAT_BITMAP =<< peek (format slot)

    left <- fromIntegral <$> peek (bitmap_left slot)
    top <- fromIntegral <$> peek (bitmap_top slot)

    infoM "Hadron" $ "Font texture position: " ++ show left ++ " x " ++ show top

    -- Get the char bitmap.
    bitmap <- peek $ bitmap slot :: IO FT_Bitmap

    let w = fromIntegral $ width bitmap
        r = fromIntegral $ rows bitmap
        p = fromIntegral $ pitch bitmap

    assertEquals (((w + 15) `div` 16) * 2) p
    assertEquals 0 (num_grays bitmap)
    assertEquals 1 (pixel_mode bitmap)
    assertEquals 0 (palette_mode bitmap)

    infoM "Hadron" $ "Font texture size: " ++ show w ++ " x " ++ show r

    -- Get the raw bitmap data.
    bitmapData <- V.fromList <$> peekArray (p * r) (buffer bitmap :: CString) :: IO (V.Vector CChar) -- CString == Ptr CChar

    return (CharInfo charCode 0 0 left top w r, createSignedDistanceField bitmapData (w, r, p))

data ImageSource = ImageSource
    {   isWitdh :: Int
    ,   isRow :: Int
    ,   isReadPixel :: Int -> Int -> Int
    }

createDistanceField :: V.Vector CChar -> (Int, Int, Int) -> ImageSource
createDistanceField bitmapData (w, r, p) = ImageSource (xEnd + 1) (yEnd + 1) readPixel where
    scale = 4
    border = 512 `div` scale
    xEnd = w + border * 2 - 1
    yEnd = r + border * 2 - 1

    -- Not signed actually.
    sdf :: UV.Vector Double
    sdf = UV.fromList $ snd $ mapAccumL findNearest Nothing circuit where
        -- We scan the lines like a snake in order to only move one cell at a time.
        circuit = [(if even y then x else xEnd - x, y) | y <- [0 .. yEnd],  x <- [0 .. xEnd]]

    findNearest :: Maybe (Point, Point) -> Point -> (Maybe (Point, Point), Double)

    findNearest Nothing p' = (Just (n', p'), getSqrt d2) where
        area = [(x, y)  | y <- [border .. yEnd - border]
                        , x <- [border .. xEnd - border]
                        ]
        (n', d2) = findMin area p'

    findNearest (Just (n@(nx, ny), p@(px, py))) p'@(px', py') = (Just (n', p'), d'') where
        (n', d'') = if py == py' && py == ny && ((px < nx) == (px < px'))
            then (n, fromIntegral $ abs (px' - nx))
            else
                let d = floor $ simpleDistance n p
                    d' = ceiling $ simpleDistance n p'
                    -- Don't search a better point farther than the previous nearest node (roughly).
                    (yMin, yMax) = (py' - d', py' + d')
                    (xMin, xMax) = (px' - d', px' + d')
                    -- The new nearest point is necessarily in the same direction as our scanning.
                    ((yMin', yMax'), (xMin', xMax'))
                        | px < px' = ((yMin, yMax), (nx, xMax))
                        | px > px' = ((yMin, yMax), (xMin, nx))
                        | otherwise = ((ny, yMax), (xMin, xMax))
                    area = [(x, y)  | y <- [(max border yMin') .. (min (yEnd - border) yMax')]
                                    , x <- [(max border xMin') .. (min (xEnd - border) xMax')]
                                    -- Don't search a better point farther than the previous nearest node.
                                    , circle p' d' (x, y) < 1
                                    -- Don't search inside the previous search area.
                                    , circle p d (x, y) >= 0
                                    ]
                    (n', d2) = findMin area p'
                in  (n', getSqrt d2)

    findMin :: [Point] -> Point -> (Point, Int)
    findMin area p = minimumBy (comparing snd) (if null area' then [(p, 0)] else area')
        where area' = mapMaybe f area
              f n = if isIn n then Just (n, squareDistance n p) else Nothing

    isIn (x, y) = testBit (bitmapData V.! ((x' `shiftR` 3) + y' * p)) (7 - (x' .&. 7))
        where (x', y') = (x - border, y - border)

    readPixel x y = min 255 . (* scale) . round $ d
        where d = sdf UV.! ((if even y then x else xEnd - x) + y * (xEnd + 1))

createSignedDistanceField :: V.Vector CChar -> (Int, Int, Int) -> ImageSource
createSignedDistanceField bitmapData (w, r, p) = ImageSource (xEnd + 1) (yEnd + 1) readPixel where
    scale = 4
    border = 512 `div` scale
    xEnd = w + border * 2 - 1
    yEnd = r + border * 2 - 1

    -- Not signed actually.
    sdf :: UV.Vector Double
    sdf = UV.fromList $ snd $ mapAccumL findNearest Nothing circuit where
        -- We scan the lines like a snake in order to only move one cell at a time.
        circuit = [(if even y then x else xEnd - x, y) | y <- [0 .. yEnd],  x <- [0 .. xEnd]]

    findNearest :: Maybe (Point, Point) -> Point -> (Maybe (Point, Point), Double)

    findNearest Nothing p' = (Just (n', p'), getSqrt d2) where
        area = [(x, y)  | y <- [border - 1 .. yEnd - border + 1]
                        , x <- [border - 1 .. xEnd - border + 1]
                        ]
        (n', d2) = findMin area p'

    findNearest (Just (n@(nx, ny), p@(px, py))) p'@(px', py') = (Just (n', p'), d''') where
        outside = is False p
        outside' = is False p'
        d''' = if outside' then d'' else 1 - d''
        (n', d'')
            | outside /= outside' = (p, 1)
            | py == py' && py == ny && ((px < nx) == (px < px')) = (n, fromIntegral $ abs (px' - nx))
            | otherwise =
                let d = floor $ simpleDistance n p
                    d' = ceiling $ simpleDistance n p'
                    -- Don't search a better point farther than the previous nearest node (roughly).
                    (yMin, yMax) = (py' - d', py' + d')
                    (xMin, xMax) = (px' - d', px' + d')
                    -- The new nearest point is necessarily in the same direction as our scanning.
                    ((yMin', yMax'), (xMin', xMax'))
                        | px < px' = ((yMin, yMax), (nx, xMax))
                        | px > px' = ((yMin, yMax), (xMin, nx))
                        | otherwise = ((ny, yMax), (xMin, xMax))
                    area = [(x, y)  | y <- [max (border - 1) yMin' .. min (yEnd - border + 1) yMax']
                                    , x <- [max (border - 1) xMin' .. min (xEnd - border + 1) xMax']
                                    -- Don't search a better point farther than the previous nearest node.
                                    , circle p' d' (x, y) < 1
                                    -- Don't look inside the previous search area.
                                    , circle p d (x, y) >= 0
                                    ]
                    (n', d2) = findMin area p'
                in  (n', getSqrt d2)

    findMin :: [Point] -> Point -> (Point, Int)
    findMin area p@(x, y) = minimumBy (comparing (abs . snd)) (if null area' then [(p, 0)] else area')
        where inside = is False p
              area' = mapMaybe f area
              f a = if is inside a then Just (a, squareDistance a p) else Nothing

    is inside (x, y) = if outside then not inside else result
        where outside = x < border || x >= w + border || y < border || y >= r + border
              result = inside == testBit (bitmapData V.! ((x' `shiftR` 3) + y' * p)) (7 - (x' .&. 7))
              (x', y') = (x - border, y - border)

    readPixel x y = min 255 . (+ 127) . round $ d
        where d = sdf UV.! ((if even y then x else xEnd - x) + y * (xEnd + 1))

----------------------------------------------------------------------------------------------------

squareDistance :: Point -> Point -> Int
squareDistance (ax, ay) (bx, by) = (ax - bx)^2 + (ay - by)^2

simpleDistance :: Point -> Point -> Double
simpleDistance a b = getSqrt (squareDistance a b)

circle :: Num a => (a, a) -> a -> (a, a) -> a
circle (cx, cy) r (x, y) = (x - cx)^2 + (y - cy)^2 - r^2

-- Very slight benefit...
cachedRoots = UV.generate 2048 (sqrt . fromIntegral) :: UV.Vector Double
getSqrt x = if x < 2048 then cachedRoots UV.! x else (sqrt . fromIntegral) x
