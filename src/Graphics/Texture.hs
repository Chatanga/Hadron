{-# language ScopedTypeVariables, FlexibleContexts, TypeFamilies, LambdaCase #-}

module Graphics.Texture
    ( loadImage
    , loadGreyscaleImage
    , loadCubeImage
    , saveDepthTexture
    , saveTexture
    , generateNoiseTexture
    , generate2DNoiseTexture
    , generate3DNoiseTexture
    , withSingleTranspose
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Exception
import Codec.Picture as JP
import Codec.Picture.Types
import Data.Array
import Data.List
import Data.Word (Word8, Word16, Word32)
import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import System.Log.Logger

import Common.Debug
import Common.Random
import Data.Vector (unfoldrExactN)

------------------------------------------------------------------------------------------------------------------------

getFormatName :: DynamicImage -> (String, String)
getFormatName dynamicImage = case dynamicImage of
    ImageY8 _       -> ("Y8",       "a greyscale image")
    ImageY16 _      -> ("Y16",      "a greyscale image with 16 bit component")
    ImageY32 _      -> ("Y32",      "a greyscale image with 32 bit component")
    ImageYF _       -> ("YF",       "a greyscale HDR image")
    ImageYA8 _      -> ("YA8",      "an image in greyscale with an alpha channel")
    ImageYA16 _     -> ("YA16",     "an image in greyscale with alpha channel on 16 bit")
    ImageRGB8 _     -> ("RGB8",     "an image in true color")
    ImageRGB16 _    -> ("RGB16",    "an image in true color with 16 bit depth")
    ImageRGBF _     -> ("RGBF",     "an image with HDR pixels")
    ImageRGBA8 _    -> ("RGBA8",    "an image in true color and an alpha channel")
    ImageRGBA16 _   -> ("RGBA16",   "a true color image with alpha on 16 bit")
    ImageYCbCr8 _   -> ("YCbCr8",   "an image in the colorspace used by Jpeg images")
    ImageCMYK8 _    -> ("CMYK8",    "an image in the colorspace CMYK")
    ImageCMYK16 _   -> ("CMYK16",   "an image in the colorspace CMYK and 16 bit precision")

loadImage :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m) => String -> ContextT ctx os m (Maybe (Texture2D os (Format RGBFloat)))
loadImage path = do
    let loadPixels3 image = do
            let size = V2 (imageWidth image) (imageHeight image)
            texture <- newTexture2D' path SRGB8 size maxBound -- JPG converts to SRGB
            let getJuicyPixel xs _x _y pix = let PixelRGB8 r g b = convertPixel pix in V3 r g b : xs
            writeTexture2D texture 0 (pure 0) size (pixelFold getJuicyPixel [] image)
            return (Just texture)

    liftIO (readImage path) >>= \case
        Left e -> do
            liftIO $ errorM "Hadron" ("could not load image " ++ path ++ ": " ++ e)
            return Nothing
        Right dynamicImage -> do
            liftIO $ infoM "Hadron" ("Loading image format " ++ path ++ ": " ++ fst (getFormatName dynamicImage))
            case dynamicImage of
                -- ImageY8 image -> loadPixels image
                ImageRGB8 image -> loadPixels3 image
                -- ImageRGBA8 image -> loadPixels image
                ImageYCbCr8 image ->
                    let image' = convertImage image :: JP.Image PixelRGB8
                    in  loadPixels3 image'
                _ -> do
                    liftIO $ errorM "Hadron" ("Unmanaged image format " ++ path ++ ": " ++ fst (getFormatName dynamicImage))
                    return Nothing

loadGreyscaleImage :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m) => String -> ContextT ctx os m (Maybe (Texture2D os (Format RFloat)))
loadGreyscaleImage path = do
    let loadPixels image = do
            let size = V2 (imageWidth image) (imageHeight image)
            texture <- newTexture2D' path R8S size maxBound
            let getJuicyPixel :: [Float] -> p -> p -> Pixel8 -> [Float]
                getJuicyPixel xs _x _y pix = let x = fromIntegral (convertPixel pix :: Pixel8) :: Float in (x / 256) : xs
            writeTexture2D texture 0 (pure 0) size (pixelFold getJuicyPixel [] image)
            return (Just texture)

    liftIO (readImage path) >>= \case
        Left e -> do
            liftIO $ errorM "Hadron" ("could not load image " ++ path ++ ": " ++ e)
            return Nothing
        Right dynamicImage -> do
            liftIO $ infoM "Hadron" ("Loading image format " ++ path ++ ": " ++ fst (getFormatName dynamicImage))
            case dynamicImage of
                ImageY8 image ->
                    let image' = convertImage image :: JP.Image Pixel8
                    in  loadPixels image'
                _ -> do
                    liftIO $ errorM "Hadron" ("Unmanaged image format " ++ path ++ ": " ++ fst (getFormatName dynamicImage))
                    return Nothing

loadGreyscaleImage2 :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m) => String -> ContextT ctx os m (Maybe (Texture2D os (Format RWord)))
loadGreyscaleImage2 path = do
    let loadPixels image = do
            let size = V2 (imageWidth image) (imageHeight image)
            texture <- newTexture2D' path R8UI size maxBound
            let getJuicyPixel :: [Word8] -> p -> p -> Pixel8 -> [Word8]
                getJuicyPixel xs _x _y pix = let x = fromIntegral (convertPixel pix :: Pixel8) :: Word8 in x : xs
            writeTexture2D texture 0 (pure 0) size (pixelFold getJuicyPixel [] image)
            return (Just texture)

    liftIO (readImage path) >>= \case
        Left e -> do
            liftIO $ errorM "Hadron" ("could not load image " ++ path ++ ": " ++ e)
            return Nothing
        Right dynamicImage -> do
            liftIO $ infoM "Hadron" ("Loading image format " ++ path ++ ": " ++ fst (getFormatName dynamicImage))
            case dynamicImage of
                ImageY8 image ->
                    let image' = convertImage image :: JP.Image Pixel8
                    in  loadPixels image'
                _ -> do
                    liftIO $ errorM "Hadron" ("Unmanaged image format " ++ path ++ ": " ++ fst (getFormatName dynamicImage))
                    return Nothing

withSingleTranspose :: Int -> ([a] -> [a]) -> ([a] -> [a]) -> [a] -> [a]
withSingleTranspose n before after = concatMap after . Data.List.transpose . map before . take n . unfoldr (Just . splitAt n)

withDualTranspose :: Int -> ([a] -> [a]) -> ([a] -> [a]) -> [a] -> [a]
withDualTranspose n before after = concat . Data.List.transpose . map after . Data.List.transpose . map before . take n . unfoldr (Just . splitAt n)

rot90_mirrorV n = withSingleTranspose n id id
rot270_mirrorH = rot90_mirrorV
rot90 n = withSingleTranspose n id reverse
rot270 n = withSingleTranspose n reverse id
rot90_mirrorH n = withSingleTranspose n reverse reverse
rot270_mirrorV = rot90_mirrorH

mirrorH n = withDualTranspose n id reverse
rot180_mirrorV = mirrorH
mirrorV n = withDualTranspose n reverse id
rot180_mirrorH = mirrorV
rot180 n = withDualTranspose n reverse reverse
mirrorVH = rot180

loadCubeImage :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m) => Int -> [String] -> ContextT ctx os m (Maybe (TextureCube os (Format RGBFloat)))
loadCubeImage expectedSize paths = do
    texture <- newTextureCube SRGB8 expectedSize maxBound -- JPG converts to SRGB

    let loadPixels cubeFace path image = do
            let size = traceIt "size" $ V2 (imageWidth image) (imageHeight image)
                getJuicyPixel xs _x _y pix = let PixelRGB8 r g b = convertPixel pix in V3 r g b : xs
                transform ps
                    | cubeFace == CubeNegX = rot270 expectedSize ps
                    | cubeFace == CubePosX = rot90 expectedSize ps
                    | cubeFace == CubeNegY = rot180 expectedSize ps
                    | cubeFace == CubePosY = ps
                    | cubeFace == CubeNegZ = ps
                    | cubeFace == CubePosZ = rot90 expectedSize ps
                    | otherwise = error "Non exhaustive? Really?"
            writeTextureCube texture 0 cubeFace (pure 0) size (transform (pixelFold getJuicyPixel [] image))
            return True

    let cubeFaces = [CubeNegX, CubePosX, CubeNegY, CubePosY, CubeNegZ, CubePosZ]

    success <- fmap and <$> forM (zip cubeFaces paths) $ \(cubeFace, path) ->
        liftIO (readImage path) >>= \case
            Left e -> do
                liftIO $ errorM "Hadron" ("could not load image " ++ path ++ ": " ++ e)
                return False
            Right dynamicImage -> do
                liftIO $ infoM "Hadron" ("Loading image format " ++ path ++ ": " ++ fst (getFormatName dynamicImage))
                case dynamicImage of
                    -- ImageY8 image -> loadPixels image
                    ImageRGB8 image -> loadPixels cubeFace path image
                    -- ImageRGBA8 image -> loadPixels image
                    ImageYCbCr8 image ->
                        let image' = convertImage image :: JP.Image PixelRGB8
                        in  loadPixels cubeFace path image'
                    _ -> do
                        liftIO $ errorM "Hadron" ("Unmanaged image format " ++ path ++ ": " ++ fst (getFormatName dynamicImage))
                        return False

    return $ if success then Just texture else Nothing

saveDepthTexture :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadAsyncException m) => (Int, Int) -> Texture2D os (Format Depth) -> String -> ContextT ctx os m ()
saveDepthTexture (w, h) texture path = do
    let level = 0
        f a p = return (p : a)
    pixels :: Array (Int, Int) Float <- listArray ((0, 0), (h-1, w-1)) <$> readTexture2D texture level (V2 0 0) (V2 w h) f []
    let
        getPixel :: Int -> Int -> PixelF
        getPixel x y = pixels ! (y, x)
        image = generateImage getPixel w h
    liftIO $ savePngImage path (ImageYF image)

saveTexture :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadAsyncException m) => (Int, Int) -> Texture2D os (Format RGBFloat) -> String -> ContextT ctx os m ()
saveTexture (w, h) texture path = do
    let level = 0
        f a p = return (p : a)
    pixels :: Array (Int, Int) (V3 Float) <- listArray ((0, 0), (h-1, w-1)) <$> readTexture2D texture level (V2 0 0) (V2 w h) f []
    let
        getPixel :: Int -> Int -> PixelRGBF
        getPixel x y = let V3 r g b = pixels ! (y, x) in PixelRGBF r g b
        image = generateImage getPixel w h
    liftIO $ savePngImage path (ImageRGBF image)

generateNoiseTexture :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m) => (Int, Int) -> ContextT ctx os m (Texture2D os (Format RGBFloat))
generateNoiseTexture (width, height) = do
    noise <- liftIO . runRandomIO $ replicateM (width * height) $ do
        x <- getRandomR (0, 1) :: RandomState Float
        y <- getRandomR (0, 1) :: RandomState Float
        return (V3 (x * 2 - 1) (y * 2 - 1) 0)
    let size = V2 width height
    texture <- newTexture2D' "2D noise" RGB16F size 1 -- maxBound
    writeTexture2D texture 0 0 size noise
    return texture

generate2DNoiseTexture :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m) => (Int, Int) -> ContextT ctx os m (Texture2D os (Format RFloat))
generate2DNoiseTexture (width, height) = do
    noise <- liftIO . runRandomIO $ replicateM (width * height) $ do
        (\x -> x * 2 - 1) <$> getRandomR (0, 1) :: RandomState Float
    let size = V2 width height
    texture <- newTexture2D' "2D noise" R16F size 1
    writeTexture2D texture 0 0 size noise
    return texture

generate3DNoiseTexture :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m) => (Int, Int, Int) -> ContextT ctx os m (Texture3D os (Format RFloat))
generate3DNoiseTexture (width, height, depth) = do
    noise <- liftIO . runRandomIO $ replicateM (width * height * depth) $ do
        (\x -> x * 2 - 1) <$> getRandomR (0, 1) :: RandomState Float
    let size = V3 width height depth
    texture <- newTexture3D' "3D noise" R16F size 1
    writeTexture3D texture 0 0 size noise
    return texture
