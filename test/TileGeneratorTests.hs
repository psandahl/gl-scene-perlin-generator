module TileGeneratorTests
    ( indices2x2
    ) where

import           Data.Vector.Storable  (fromList)
import           Scene.PerlinGenerator
import           Test.HUnit

-- | Test generated indices for a grid of 2 x 2 vertices.
indices2x2 :: Assertion
indices2x2 =
    fromList [1, 0, 2, 1, 2, 3] @=? generateIndices 2 2
