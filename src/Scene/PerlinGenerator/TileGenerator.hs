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

generateIndices :: Int -> Int -> Vector GLuint
generateIndices = undefined
