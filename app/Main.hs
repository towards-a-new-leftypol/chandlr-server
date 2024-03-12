{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Miso (ToServerRoutes, View, h1_, text)
import           Data.Proxy
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp             as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai
import Servant.API
import Servant.Server (Server, Handler (..), serve)
import qualified Lucid      as L
import qualified Lucid.Base as L


import qualified Common.FrontEnd.Routes as FE
import qualified Common.FrontEnd.Action as FE
import qualified Common.FrontEnd.Model  as FE

newtype HtmlPage a = HtmlPage a

instance (L.ToHtml a) => L.ToHtml (HtmlPage a) where
    toHtmlRaw = L.toHtml
    toHtml (HtmlPage x) = L.toHtml x
    -- toHtml (HtmlPage x) = do
    --     L.doctype_
    --     L.head_ $ do
    --       L.title_ "Chandlr"
    --       L.meta_ [L.charset_ "utf-8"]

    --       L.with (L.script_ mempty)
    --         [ L.makeAttribute "src" "/static/all.js"
    --         , L.makeAttribute "async" mempty
    --         , L.makeAttribute "defer" mempty
    --         ]

    --     L.body_ (L.toHtml x)

type FrontEndRoutes = ToServerRoutes FE.Route HtmlPage FE.Action

handlers :: Server FrontEndRoutes
handlers = catalogView :<|> threadView :<|> searchView

catalogView :: Handler (HtmlPage (View FE.Action))
catalogView = do
    liftIO $ putStrLn "Hello World"

    pure $ HtmlPage $ h1_ [] [ text "Hello World" ]

{-
    return $ HtmlPage $ do
        L.doctype_
        L.head_ $ do
          L.title_ "Chandlr"
          L.meta_ [L.charset_ "utf-8"]

          L.with (L.script_ mempty)
            [ L.makeAttribute "src" "/static/all.js"
            , L.makeAttribute "async" mempty
            , L.makeAttribute "defer" mempty
            ]

        L.body_ (L.h1_ "Hello World")
-}

threadView :: Text -> Text -> FE.BoardThreadId -> Handler (HtmlPage (View FE.Action))
threadView = undefined

searchView :: Maybe Text -> Handler (HtmlPage (View FE.Action))
searchView = undefined

viewPage :: FE.Model -> View FE.Action
viewPage = undefined

app :: Wai.Application
app = serve (Proxy @FrontEndRoutes) handlers

port :: Int
port = 8888

main :: IO ()
main = do
    putStrLn $ "Serving front-end on port " ++ show port

    Wai.run port $ Wai.logStdout app
