-- |
-- Module: Scene.PerlinGenerator.GeneratorContext
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Scene.PerlinGenerator.GeneratorContext
    ( GeneratorContext (..)
    , defaultGeneratorContext
    , singletonWeight
    ) where

import           Scene.Math                             (Perlin, Weight (..),
                                                         initPerlin)
import           Scene.PerlinGenerator.WeightGenerators (singletonWeight)

-- | A set of parameters for the generation of Perlin data.
data GeneratorContext = GeneratorContext
    { xDividend :: !Double
    -- ^ Constant used for division of the x parameter. I.e. how far the x
    -- value have to go before x / dividend reaches an integer.
    , yDividend :: !Double
    -- ^ Constant used for division of the y parameter. I.e. how far the x
    -- value have to go before y / dividend reaches an integer.
    , weights   :: ![Weight]
    -- ^ A list of weights; pair of frequence (which x and y will be multiplied with)
    -- and altitude (used to multiply the resulting height in each step).
    , perlin    :: !Perlin
    -- ^ The perlin permutation table.
    } deriving Show

-- | Generate a default generator context record.
defaultGeneratorContext :: GeneratorContext
defaultGeneratorContext =
    GeneratorContext
        { xDividend = 1024
        , yDividend = 1024
        , weights = singletonWeight
        , perlin = initPerlin
        }
