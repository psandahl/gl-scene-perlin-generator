{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import           Data.Map.Strict                      (Map)
import qualified Data.Map.Strict                      as Map
import           Data.Text.Lazy                       (Text)
import           Flow                                 ((<|))
import           Network.HTTP.Types                   (badRequest400)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Scene.Math                           (Weight (..))
import           Scene.PerlinGenerator
import           Web.Scotty

type WeightMap = Map Text [Weight]

main :: IO ()
main = do
    let context = defaultGeneratorContext
        weightMap = makeWeightMap
    scotty 8000 $ do
        middleware logStdoutDev

        get "/image/:selection" <| genImage context weightMap `rescue` badRequest

genImage :: GeneratorContext -> WeightMap -> ActionM ()
genImage context weightMap = do
    selection <- param "selection"
    case Map.lookup selection weightMap of
        Just ws -> do
            query <- generatorQuery
            setHeader "Content-Type" "image/png"
            raw <| genSerializedImageRGB8 (context { weights = ws }) query

        Nothing -> raise "Cannot find weight map"

badRequest :: Text -> ActionM ()
badRequest msg = do
    status badRequest400
    text msg

generatorQuery :: ActionM GeneratorQuery
generatorQuery =
    GeneratorQuery <$> param "xPos"
                   <*> param "yPos"
                   <*> pure 1024
                   <*> pure 1024
                   <*> pure 255

makeWeightMap :: WeightMap
makeWeightMap =
    Map.fromList
        [ ("singleton", singletonWeight)
        , ("cloudy-sky", cloudySky)
        ]
