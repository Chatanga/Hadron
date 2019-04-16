{-# LANGUAGE Arrows, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables, PackageImports, TypeFamilies #-}

module Graphics.Shader
    ( DirectionLight(..)
    , DirectionLightB(..)
    , DirectionLightS(..)
    , PointLight(..)
    , PointLightB(..)
    , PointLightS(..)
    , Fog(..)
    , FogB(..)
    , FogS(..)
    , ShaderConfig(..)
    , ShaderConfigB(..)
    , ShaderConfigS(..)
    ) where

import Control.Arrow
import Data.Int

import Linear
import Graphics.GPipe

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
