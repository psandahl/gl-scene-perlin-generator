{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import           Data.Text.Lazy                       (Text)
import           Flow                                 ((<|))
import           Network.HTTP.Types                   (badRequest400)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Scene.PerlinGenerator
import           Web.Scotty

main :: IO ()
main =
    scotty 8000 $ do
        middleware logStdoutDev

        get "/image/default" <| genImage defaultGeneratorContext `rescue` badRequest

genImage :: GeneratorContext -> ActionM ()
genImage context = do
    query <- generatorQuery
    setHeader "Content-Type" "image/png"
    raw <| genSerializedImageRGB8 context query

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
