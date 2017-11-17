-- |
-- Module: Scene.PerlinGenerator
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Scene.PerlinGenerator
    ( genImageRGBA8
    , genSerializedImageRGBA8
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

genImageRGBA8 :: GeneratorContext -> GeneratorQuery -> Image PixelRGBA8
genImageRGBA8 context query =
    generateImage (\x -> toColor . perlinValue context query x)
                  (width query) (height query)

genSerializedImageRGBA8 :: GeneratorContext -> GeneratorQuery -> ByteString
genSerializedImageRGBA8 context = encodePng . genImageRGBA8 context

-- | Generate a color from the vectors y value. The y value goes from everything
-- from rgb to alpha.
toColor :: RealFrac a => V3 a -> PixelRGBA8
toColor vec =
    let val = round <| vec ^. _y
    in PixelRGBA8 val val val val
