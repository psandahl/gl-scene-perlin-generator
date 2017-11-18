module Main
    ( main
    ) where

import           Test.Framework                 (Test, defaultMain, testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           TileGeneratorTests             (indices2x2)

main :: IO ()
main = defaultMain testSuite

testSuite :: [Test]
testSuite =
    [ testGroup "TileData generation"
        [ testCase "Index generation for 2x2" indices2x2
        ]
    ]
