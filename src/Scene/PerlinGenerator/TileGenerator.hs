{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
-- |
-- Module: Scene.PerlinGenerator.TileGenerator
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module Scene.PerlinGenerator.TileGenerator
    ( TileData (..)
    , generateVertices
    , generateIndices
    ) where

import           Control.DeepSeq                      (NFData)
import           Data.Vector.Storable                 (Vector)
import qualified Data.Vector.Storable                 as Vector
import           GHC.Generics                         (Generic)
import           Linear                               (V3)
import           Scene                                (GLfloat, GLuint)
import           Scene.GL.Attribute.VertexWithPosNorm (Vertex (..))

data TileData = TileData
    { tileWidth  :: !Int
    , tileHeight :: !Int
    , startX     :: !Int
    , startZ     :: !Int
    , vertices   :: !(Vector Vertex)
    , indices    :: !(Vector GLuint)
    } deriving (Generic, NFData, Show)

generateVertices :: (Int -> Int -> V3 GLfloat) -> Int -> Int -> Vector Vertex
generateVertices = undefined

-- | Generate indices for a grid of width x height vertices.
generateIndices :: Int -> Int -> Vector GLuint
generateIndices w d =
    Vector.concatMap (\row ->
        Vector.concatMap (\col ->
            let upperLeft = row * w + col
                upperRight = upperLeft + 1
                lowerLeft = (row + 1) * w + col
                lowerRight = lowerLeft + 1
            in Vector.fromList [ fromIntegral upperRight
                               , fromIntegral upperLeft
                               , fromIntegral lowerLeft
                               , fromIntegral upperRight
                               , fromIntegral lowerLeft
                               , fromIntegral lowerRight
                               ]
        ) (Vector.fromList [0 .. w - 2])
    ) (Vector.fromList [0 .. d - 2])
