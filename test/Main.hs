module Main
    ( main
    ) where

import           Test.Framework                 (Test, defaultMain, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           TileGeneratorTests             (generatedVertices, indices2x2,
                                                 indices2x3, indices3x2)

main :: IO ()
main = defaultMain testSuite

testSuite :: [Test]
testSuite =
    [ testGroup "TileData generation"
        [ testCase "Index generation for 2x2" indices2x2
        , testCase "Index generation for 3x2" indices3x2
        , testCase "Index generation for 2x3" indices2x3
        , testCase "Generated vertices" generatedVertices
        ]
    ]
