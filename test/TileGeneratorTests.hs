module TileGeneratorTests
    ( indices2x2
    , indices3x2
    , indices2x3
    , generatedVertices
    ) where

import           Data.Vector.Storable                 (fromList)
import           Flow                                 ((<|))
import           Linear                               (V3 (..), normalize)
import           Scene                                (GLfloat)
import           Scene.GL.Attribute.VertexWithPosNorm (Vertex (..))
import           Scene.PerlinGenerator
import           Test.HUnit

-- | Test generated indices for a grid of 2 x 2 vertices.
indices2x2 :: Assertion
indices2x2 =
    fromList [1, 0, 2, 1, 2, 3] @=? generateIndices 2 2

-- | Test generated indices for a grid of 3 x 2 vertices.
indices3x2 :: Assertion
indices3x2 =
    fromList [ 1, 0, 3, 1, 3, 4
             , 2, 1, 4, 2, 4, 5
             ] @=? generateIndices 3 2

-- | Test generated indices for a grid of 2 x 3 vertices.
indices2x3 :: Assertion
indices2x3 =
    fromList [ 1, 0, 2, 1, 2, 3
             , 3, 2, 4, 3, 4, 5
             ] @=? generateIndices 2 3

-- | Check the generated vertices. Positions shall be the expected and normals
-- shall be smoothed and normalized.
generatedVertices :: Assertion
generatedVertices =
    fromList [ Vertex { position = V3 0 0 0, normal = normalize <| V3 (-1) 1 (-1) }
             , Vertex { position = V3 1 1 0, normal = V3 0 1 0 }
             , Vertex { position = V3 0 1 1, normal = V3 0 1 0 }
             , Vertex { position = V3 1 0 1, normal = normalize <| V3 1 1 1 }
             ] @=? (smoothNormals (generateIndices 2 2) <| generateVertices perlinValue 2 2)

perlinValue :: Int -> Int -> V3 GLfloat
perlinValue x z =
    case (x, z) of
        (0, 0) -> V3 (fromIntegral x) 0 (fromIntegral z)
        (1, 0) -> V3 (fromIntegral x) 1 (fromIntegral z)
        (0, 1) -> V3 (fromIntegral x) 1 (fromIntegral z)
        (1, 1) -> V3 (fromIntegral x) 0 (fromIntegral z)
        _      -> error "Unexpected coordinates"
