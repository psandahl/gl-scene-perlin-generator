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
    , smoothNormals
    ) where

import           Control.DeepSeq                      (NFData)
import           Control.Monad                        (forM_)
import           Control.Monad.Primitive              (PrimMonad)
import           Control.Monad.ST                     (runST)
import           Data.Vector.Storable                 (Vector, (!?))
import qualified Data.Vector.Storable                 as Vector
import qualified Data.Vector.Storable.Mutable         as MVector
import           GHC.Generics                         (Generic)
import           Linear                               (V3, normalize, zero)
import           Scene                                (GLfloat, GLuint)
import           Scene.GL.Attribute.VertexWithPosNorm (Vertex (..))
import           Scene.Math                           (surfaceNormal)

data TileData = TileData
    { tileWidth  :: !Int
    , tileHeight :: !Int
    , startX     :: !Int
    , startZ     :: !Int
    , vertices   :: !(Vector Vertex)
    , indices    :: !(Vector GLuint)
    } deriving (Generic, NFData, Show)

-- | Generate width * height number of vertices. Use the provided function to
-- generate the vertex position.
generateVertices :: (Int -> Int -> V3 GLfloat) -> Int -> Int -> Vector Vertex
generateVertices g width height = Vector.generate (width * height) mkVertex
    where
        mkVertex i =
            let (x, z) = fromLinearIndex width i
            in Vertex
                 { position = g x z
                 , normal = zero
                 }

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

-- | Generate smooth normals for the vertices.
smoothNormals :: Vector GLuint -> Vector Vertex -> Vector Vertex
smoothNormals indices' inputVertices =
    runST $ do
        mutableVertices <- Vector.unsafeThaw inputVertices

        -- Per surface, calculate its normal and add that surface normal
        -- to each vertice.
        perSurface
            (\(i1, i2, i3) -> do
                v1 <- MVector.read mutableVertices i1
                v2 <- MVector.read mutableVertices i2
                v3 <- MVector.read mutableVertices i3

                let sn = surfaceNormal (position v1)
                                       (position v2)
                                       (position v3)

                MVector.write mutableVertices i1 $ v1 { normal = normal v1 + sn }
                MVector.write mutableVertices i2 $ v2 { normal = normal v2 + sn }
                MVector.write mutableVertices i3 $ v3 { normal = normal v3 + sn }
            ) indices'

        -- Traverse all vertices and normalize their normals.
        forM_ [0 .. MVector.length mutableVertices - 1] $
            MVector.modify mutableVertices (\v -> v { normal = normalize (normal v)})

        Vector.unsafeFreeze mutableVertices

perSurface :: PrimMonad m => ((Int, Int, Int) -> m ()) -> Vector GLuint -> m ()
perSurface g indexVector = go 0
    where
        go baseIndex =
            case (,,) <$> indexVector !? baseIndex
                      <*> indexVector !? (baseIndex + 1)
                      <*> indexVector !? (baseIndex + 2) of
                Just (i1, i2, i3)  -> do
                    g (fromIntegral i1, fromIntegral i2, fromIntegral i3)
                    go (baseIndex + 3)

                Nothing -> return ()

fromLinearIndex :: Int -> Int -> (Int, Int)
fromLinearIndex w i =
    (i `mod` w, i `div` w)
