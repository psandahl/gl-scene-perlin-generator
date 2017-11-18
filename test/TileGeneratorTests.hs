module TileGeneratorTests
    ( indices2x2
    , indices3x2
    , indices2x3
    ) where

import           Data.Vector.Storable  (fromList)
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
