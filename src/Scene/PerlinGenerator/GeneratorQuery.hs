-- |
-- Module: Scene.PerlinGenerator.GeneratorQuery
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Scene.PerlinGenerator.GeneratorQuery
    ( GeneratorQuery (..)
    ) where

data GeneratorQuery = GeneratorQuery
    { xPos   :: !Int
    , yPos   :: !Int
    , width  :: !Int
    , height :: !Int
    , scale  :: !Int
    } deriving Show
