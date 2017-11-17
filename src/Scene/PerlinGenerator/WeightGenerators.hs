-- |
-- Module: Scene.PerlinGenerator.WeightGenerators
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Scene.PerlinGenerator.WeightGenerators
    ( singletonWeight
    , logarithmicClouds
    , cloudySky
    ) where

import           Flow       ((<|))
import           Scene.Math (Weight (..))

-- | An identity weight, will when using the composed algoritm, produce the
-- same result as the basic algorithm.
singletonWeight :: [Weight]
singletonWeight = [ Weight 1 1 ]

-- | Generate clouds using n number of octaves. For each octave the frequency
-- is doubled and the altitude is cut in half.
logarithmicClouds :: Int -> Weight -> [Weight]
logarithmicClouds n startWeight = go n startWeight []
    where
        go :: Int -> Weight -> [Weight] -> [Weight]
        go 0 _ ws  = reverse ws
        go it w ws = go (it - 1) (skew w) (w:ws)

        skew :: Weight -> Weight
        skew (Weight f a) = Weight (f * 2) (a * 0.5)

-- | Convenience function for a nice cloudy sky.
cloudySky :: [Weight]
cloudySky = logarithmicClouds 10 <| Weight 1 0.5
