-- |
-- Module: Scene.PerlinGenerator
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Scene.PerlinGenerator
    ( genImageRGB8
    , genSerializedImageRGB8
    , module Scene.PerlinGenerator.GeneratorContext
    , module Scene.PerlinGenerator.GeneratorQuery
    ) where

import           Codec.Picture
import           Control.Lens                           ((^.))
import           Data.ByteString.Lazy                   (ByteString)
import           Flow                                   ((<|))
import           Linear                                 (V3, _y)
import           Scene.PerlinGenerator.Algo             (perlinValue)
import           Scene.PerlinGenerator.GeneratorContext
import           Scene.PerlinGenerator.GeneratorQuery

genImageRGB8 :: GeneratorContext -> GeneratorQuery -> Image PixelRGB8
genImageRGB8 context query =
    generateImage (\x -> toColor . perlinValue context query x)
                  (width query) (height query)

genSerializedImageRGB8 :: GeneratorContext -> GeneratorQuery -> ByteString
genSerializedImageRGB8 context = encodePng . genImageRGB8 context

-- | Generate a color from the vectors y value
toColor :: RealFrac a => V3 a -> PixelRGB8
toColor vec =
    let val = round <| vec ^. _y
    in PixelRGB8 val val val
