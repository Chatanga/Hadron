{-# LANGUAGE ScopedTypeVariables, PackageImports, FlexibleContexts, TypeFamilies, LambdaCase #-}

module Graphics.Texture
    ( loadImage
    , saveDepthTexture
    , saveTexture
    , generateNoiseTexture
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Array

import System.Log.Logger
import Codec.Picture as JP
import Codec.Picture.Types
import Graphics.GPipe
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW

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

loadImage :: String -> ContextT GLFW.Handle os IO (Maybe (Texture2D os (Format RGBFloat)))
loadImage path = do
    let loadPixels image = do
            let size = V2 (imageWidth image) (imageHeight image)
            texture <- newTexture2D SRGB8 size maxBound -- JPG converts to SRGB
            let getJuicyPixel xs _x _y pix = let PixelRGB8 r g b = convertPixel pix in V3 r g b : xs   
            writeTexture2D texture 0 0 size (pixelFold getJuicyPixel [] image)
            return (Just texture)

    (liftIO $ readImage path) >>= \case
        Left e -> do
            liftIO $ errorM "Kage" ("could not load image " ++ path ++ ": " ++ e)
            return Nothing
        Right dynamicImage -> do
            liftIO $ infoM "Kage" ("Loading image format " ++ path ++ ": " ++ fst (getFormatName dynamicImage))
            case dynamicImage of
                -- ImageY8 image -> loadPixels image
                ImageRGB8 image -> loadPixels image
                -- ImageRGBA8 image -> loadPixels image
                ImageYCbCr8 image ->
                    let image' = convertImage image :: JP.Image PixelRGB8
                    in  loadPixels image'
                _ -> do
                    liftIO $ errorM "Kage" ("Unmanaged image format " ++ path ++ ": " ++ fst (getFormatName dynamicImage))
                    return Nothing

saveDepthTexture :: (Int, Int) -> Texture2D os (Format Depth) -> String -> ContextT GLFW.Handle os IO ()
saveDepthTexture (w, h) texture path = do
    let level = 0
        f :: [Float] -> HostFormat (BufferColor Float Float) -> ContextT GLFW.Handle os IO [Float]
        f a p = return (p : a)
    pixels :: Array (Int, Int) Float <- listArray ((0, 0), (h-1, w-1)) <$> readTexture2D texture level (V2 0 0) (V2 w h) f []
    let
        getPixel :: Int -> Int -> PixelF
        getPixel x y = pixels ! (y, x)
        image = generateImage getPixel w h
    liftIO $ savePngImage path (ImageYF image)

saveTexture :: (Int, Int) -> Texture2D os (Format RGBFloat) -> String -> ContextT GLFW.Handle os IO ()
saveTexture (w, h) texture path = do
    let level = 0
        f :: [V3 Float] -> HostFormat (BufferColor (V3 Float) (V3 Float)) -> ContextT GLFW.Handle os IO [V3 Float]
        f a p = return (p : a)
    pixels :: Array (Int, Int) (V3 Float) <- listArray ((0, 0), (h-1, w-1)) <$> readTexture2D texture level (V2 0 0) (V2 w h) f []
    let
        getPixel :: Int -> Int -> PixelRGBF
        getPixel x y = let V3 r g b = pixels ! (y, x) in PixelRGBF r g b
        image = generateImage getPixel w h
    liftIO $ savePngImage path (ImageRGBF image)

generateNoiseTexture :: (Int, Int) -> ContextT GLFW.Handle os IO (Texture2D os (Format RGBFloat))
generateNoiseTexture (width, height) = do
    noise <- liftIO $ replicateM (width * height) $ do
        [x, y] <- replicateM 2 (runRandomIO $ getRandomR (0, 1)) :: IO [Float]
        return (V3 (x * 2 - 1) (y * 2 - 1) 0)
    let size = V2 width height
    texture <- newTexture2D RGB16F size maxBound
    writeTexture2D texture 0 0 size noise
    return texture
