-- |
-- Module: Scene.PerlinGenerator
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Scene.PerlinGenerator
    ( TileData (..)
    , genImageRGB8
    , genSerializedImageRGB8
    , genTileData
    , module Scene.PerlinGenerator.GeneratorContext
    , module Scene.PerlinGenerator.GeneratorQuery
    , module Scene.PerlinGenerator.WeightGenerators
    , module Scene.PerlinGenerator.TileGenerator
    ) where

import           Codec.Picture
import           Control.Lens                           ((^.))
import           Data.ByteString.Lazy                   (ByteString)
import           Flow                                   ((<|))
import           Linear                                 (V3, _y)
import           Scene.PerlinGenerator.Algo             (perlinValue)
import           Scene.PerlinGenerator.GeneratorContext
import           Scene.PerlinGenerator.GeneratorQuery
import           Scene.PerlinGenerator.TileGenerator
import           Scene.PerlinGenerator.WeightGenerators

genImageRGB8 :: GeneratorContext -> GeneratorQuery -> Image PixelRGB8
genImageRGB8 context query =
    generateImage (\x -> toColor . perlinValue context query x)
                  (width query) (height query)

genSerializedImageRGB8 :: GeneratorContext -> GeneratorQuery -> ByteString
genSerializedImageRGB8 context = encodePng . genImageRGB8 context

genTileData :: GeneratorContext -> GeneratorQuery -> TileData
genTileData context query =
    let vertices' = generateVertices (perlinValue context query)
                                     (1 + width query) (1 + height query)
        indices' = generateIndices (1 + width query) (1 + height query)
    in
        TileData
            { tileWidth = width query
            , tileHeight = height query
            , startX = xPos query
            , startZ = yPos query
            , vertices = vertices'
            , indices = indices'
            }

-- | Generate a color from the vectors y value
toColor :: RealFrac a => V3 a -> PixelRGB8
toColor vec =
    let val = round <| vec ^. _y
    in PixelRGB8 val val val
