-- |
-- Module: Scene.PerlinGenerator.Algo
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Scene.PerlinGenerator.Algo
    ( perlinValue
    ) where

import           Linear                                 (V3 (..))
import           Scene                                  (GLfloat)
import           Scene.Math                             (composedNoise2D,
                                                         normalizeToGLfloat)
import           Scene.PerlinGenerator.GeneratorContext (GeneratorContext (..))
import           Scene.PerlinGenerator.GeneratorQuery   (GeneratorQuery (..))

-- | Workhorse function. From the context and a pair of coordinates
-- (starting at 0, 0) produce a V3.
-- In the resulting vector the incoming x and y become x and z components, the
-- resulting value will be the y component.
perlinValue :: GeneratorContext -> GeneratorQuery -> Int -> Int -> V3 GLfloat
perlinValue context query x y =
    let xOffset   = xPos query + x
        yOffset   = yPos query + y
        xFrac     = fromIntegral xOffset / xDividend context
        yFrac     = fromIntegral yOffset / yDividend context
        val       = composedNoise2D (perlin context) xFrac yFrac (weights context)
        valScaled = fromIntegral (scale query) * normalizeToGLfloat val
    in V3 (fromIntegral x) valScaled (fromIntegral y)
{-# INLINE perlinValue #-}
