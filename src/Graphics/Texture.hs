{-# language ScopedTypeVariables, FlexibleContexts, TypeFamilies, LambdaCase #-}

module Graphics.Texture
    ( loadImage
    , saveDepthTexture
    , saveTexture
    , generateNoiseTexture
    , generate2DNoiseTexture
    , generate3DNoiseTexture
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Exception
import Codec.Picture as JP
import Codec.Picture.Types
import Data.Array
import Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as GLFW
import System.Log.Logger

import Common.Random

------------------------------------------------------------------------------------------------------------------------

getFormatName :: DynamicImage -> (String, String)
getFormatName dynamicImage = case dynamicImage of
    ImageY8 _       -> ("Y8",       "a greyscale image")
    ImageY16 _      -> ("Y16",      "a greyscale image with 16bit components")
    ImageYF _       -> ("YF",       "a greyscale HDR image")
    ImageYA8 _      -> ("YA8",      "an image in greyscale with an alpha channel")
    ImageYA16 _     -> ("YA16",     "an image in greyscale with alpha channel on 16 bits")
    ImageRGB8 _     -> ("RGB8",     "an image in true color")
    ImageRGB16 _    -> ("RGB16",    "an image in true color with 16bit depth")
    ImageRGBF _     -> ("RGBF",     "an image with HDR pixels")
    ImageRGBA8 _    -> ("RGBA8",    "an image in true color and an alpha channel")
    ImageRGBA16 _   -> ("RGBA16",   "a true color image with alpha on 16 bits")
    ImageYCbCr8 _   -> ("YCbCr8",   "an image in the colorspace used by Jpeg images")
    ImageCMYK8 _    -> ("CMYK8",    "an image in the colorspace CMYK")
    ImageCMYK16 _   -> ("CMYK16",   "an image in the colorspace CMYK and 16 bits precision")

loadImage :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m) => String -> ContextT ctx os m (Maybe (Texture2D os (Format RGBFloat)))
loadImage path = do
    let loadPixels image = do
            let size = V2 (imageWidth image) (imageHeight image)
            texture <- newTexture2D' path SRGB8 size maxBound -- JPG converts to SRGB
            let getJuicyPixel xs _x _y pix = let PixelRGB8 r g b = convertPixel pix in V3 r g b : xs
            writeTexture2D texture 0 0 size (pixelFold getJuicyPixel [] image)
            return (Just texture)

    liftIO (readImage path) >>= \case
        Left e -> do
            liftIO $ errorM "Hadron" ("could not load image " ++ path ++ ": " ++ e)
            return Nothing
        Right dynamicImage -> do
            liftIO $ infoM "Hadron" ("Loading image format " ++ path ++ ": " ++ fst (getFormatName dynamicImage))
            case dynamicImage of
                -- ImageY8 image -> loadPixels image
                ImageRGB8 image -> loadPixels image
                -- ImageRGBA8 image -> loadPixels image
                ImageYCbCr8 image ->
                    let image' = convertImage image :: JP.Image PixelRGB8
                    in  loadPixels image'
                _ -> do
                    liftIO $ errorM "Hadron" ("Unmanaged image format " ++ path ++ ": " ++ fst (getFormatName dynamicImage))
                    return Nothing

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
    noise <- liftIO $ replicateM (width * height) $ do
        [x, y] <- replicateM 2 (runRandomIO $ getRandomR (0, 1)) :: IO [Float]
        return (V3 (x * 2 - 1) (y * 2 - 1) 0)
    let size = V2 width height
    texture <- newTexture2D' "2D noise" RGB16F size 1 -- maxBound
    writeTexture2D texture 0 0 size noise
    return texture

generate2DNoiseTexture :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m) => (Int, Int) -> ContextT ctx os m (Texture2D os (Format RFloat))
generate2DNoiseTexture (width, height) = do
    noise <- liftIO $ replicateM (width * height) $ do
        runRandomIO $ (\x -> x * 2 - 1) <$> getRandomR (0, 1) :: IO Float
    let size = V2 width height
    texture <- newTexture2D' "2D noise" R16F size 1
    writeTexture2D texture 0 0 size noise
    return texture

generate3DNoiseTexture :: forall ctx os m. (ContextHandler ctx, MonadIO m, MonadException m) => (Int, Int, Int) -> ContextT ctx os m (Texture3D os (Format RFloat))
generate3DNoiseTexture (width, height, depth) = do
    noise <- liftIO $ replicateM (width * height * depth) $ do
        runRandomIO $ (\x -> x * 2 - 1) <$> getRandomR (0, 1) :: IO Float
    let size = V3 width height depth
    texture <- newTexture3D' "3D noise" R16F size 1
    writeTexture3D texture 0 0 size noise
    return texture
