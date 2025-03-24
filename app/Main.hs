{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import System.Exit (exitFailure)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack)
import Data.Text.Lazy (toStrict)
import Miso (ToServerRoutes, View)
import Miso.String (toMisoString)
import           Data.Proxy
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp             as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai
import Servant.API
import qualified Servant
import Servant.Server
    ( Server
    , Handler (..)
    , serve
    , err500
    , err404
    , ServerError (..)
    )
import qualified Lucid      as L
import qualified Lucid.Base as L
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.UTF8 (fromString)
import System.Console.CmdArgs (cmdArgs, Data, Typeable)
import Data.Aeson (decode, ToJSON)
import Data.Aeson.Text (encodeToLazyText)
import Data.Time.Clock (getCurrentTime)
import Control.Monad.Except (throwError)
import Data.Time.Clock (UTCTime)


import JSONSettings
import qualified Common.FrontEnd.Routes as FE
import qualified Common.FrontEnd.Action as FE
import qualified Common.FrontEnd.Model  as FE
import qualified Common.FrontEnd.Views  as FE
import qualified DataClient as Client
import qualified Common.Network.ClientTypes as Client
import qualified Common.Server.JSONSettings as S
import Common.Network.CatalogPostType (CatalogPost)
import qualified Common.Component.CatalogGrid as Grid
import qualified Common.Component.TimeControl as TC
import Common.FrontEnd.Action (GetThreadArgs (..))
import qualified Common.Component.Thread.Model as Thread
import qualified Common.Component.ThreadView as Thread
import Common.Network.SiteType (Site)

data HtmlPage a = forall b. (ToJSON b) => HtmlPage (JSONSettings, b, a)

instance (L.ToHtml a) => L.ToHtml (HtmlPage a) where
    toHtmlRaw = L.toHtml
    toHtml (HtmlPage (settings, initial_data, x)) = do
        L.doctype_
        L.head_ $ do
            L.meta_ [L.charset_ "utf-8"]
            L.meta_ [L.name_ "viewport", L.content_ "width=device-width, initial-scale=1.0"]
            L.meta_ [L.name_ "postgrest-root", L.content_ (pack $ postgrest_url settings)]
            L.meta_ [L.name_ "postgrest-fetch-count", L.content_ (pack $ show $ postgrest_fetch_count settings)]
            L.meta_ [L.name_ "media-root", L.content_ (pack $ media_root settings)]
            L.meta_ [L.name_ "initial-data", L.content_ (toStrict $ encodeToLazyText initial_data) ]

            L.title_ "Chandlr"

            L.link_ [L.rel_ "stylesheet", L.href_ $ static_root <> "/static/style.css"]
            L.script_ [L.src_ $ static_root <> "/dist/build/chandlr/chandlr.jsexe/rts.js", L.makeAttribute "language" "javascript"] ("" :: B.ByteString)
            L.script_ [L.src_ $ static_root <> "/dist/build/chandlr/chandlr.jsexe/lib.js", L.makeAttribute "language" "javascript"] ("" :: B.ByteString)
            L.script_ [L.src_ $ static_root <> "/dist/build/chandlr/chandlr.jsexe/out.js", L.makeAttribute "language" "javascript"] ("" :: B.ByteString)

            L.body_ (L.toHtml x)

            L.script_
                [ L.src_ $ static_root <> "/dist/build/chandlr/chandlr.jsexe/runmain.js"
                , L.makeAttribute "language" "javascript"
                , L.makeAttribute "defer" mempty
                ]
                ("" :: B.ByteString)

        where
            static_root = pack $ static_serve_url_root settings

type FrontEndRoutes = ToServerRoutes FE.Route HtmlPage FE.Action
{-
    :Created By:
      ___________                     _____    ______ 
     /\________  \                   /\  __`\ /\_____\ 
     \/_______//'/'         __   _ __\ \ \/\ \\/_____/ 
             //'/'        /'__`\/\`'__\ \ \ \ \  
            //'/'_______ /\  __/\ \ \/ \ \ \_\ \ 
            /\___________\ \____\\ \_\  \ \_____\
            \/___________/\/____/ \/_/   \/_____/             
-}
 
 

type StaticRoute = "static" :> Servant.Raw

type API = StaticRoute :<|> FrontEndRoutes

handlers :: JSONSettings -> Server FrontEndRoutes
handlers settings
    =    (catalogView settings)
    :<|> (threadView settings)
    :<|> searchView

clientSettings :: JSONSettings -> S.JSONSettings
clientSettings (JSONSettings {..}) = S.JSONSettings
    { S.postgrest_url = postgrest_url
    , S.jwt = pack jwt
    , S.backup_read_root = undefined
    , S.media_root_path = undefined
    , S.site_name = undefined
    , S.site_url = undefined
    }

clientModel :: JSONSettings -> Client.Model
clientModel (JSONSettings {..}) = Client.Model
    { Client.pgApiRoot = pack postgrest_url
    , Client.fetchCount = postgrest_fetch_count
    }

catalogView :: JSONSettings -> Handler (HtmlPage (View FE.Action))
catalogView settings = do
    now <- liftIO $ getCurrentTime

    catalog_results <- liftIO $ do

        Client.fetchLatest
            (clientSettings settings)
            (clientModel settings)
            now

    case catalog_results of
        Left err -> throwError $ err500 { errBody = fromString $ show err }
        Right posts -> pure $ render now posts

    where
        render :: UTCTime -> [ CatalogPost ] -> HtmlPage (View FE.Action)
        render t posts = HtmlPage (settings, posts, FE.catalogView model)
            where
                model = FE.Model
                    { FE.grid_model = grid_model
                    , FE.client_model = undefined
                    , FE.thread_model = undefined
                    , FE.current_uri = undefined
                    , FE.media_root_ = undefined
                    , FE.current_time = t
                    , FE.tc_model = tc_model
                    , FE.search_model = undefined
                    }

                grid_model = Grid.Model
                    { Grid.display_items = posts
                    , Grid.media_root = toMisoString $ media_root settings
                    }

                tc_model = TC.Model 0

threadView :: JSONSettings -> Text -> Text -> FE.BoardThreadId -> Handler (HtmlPage (View FE.Action))
threadView settings website board_pathpart board_thread_id = do
    thread_results <- liftIO $ do

        Client.getThread
            (clientSettings settings)
            (clientModel settings)
            (GetThreadArgs {..})

    now <- liftIO getCurrentTime

    case thread_results of
        Left err -> throwError $ err500 { errBody = fromString $ show err }
        Right site -> do
            let s = head site
            posts_and_bodies <- liftIO $ Thread.getPostWithBodies s
            pure $ render posts_and_bodies now s

    where
        render :: [ Thread.PostWithBody ] -> UTCTime -> Site -> HtmlPage (View FE.Action)
        render posts_and_bodies t site =
            HtmlPage
                ( settings
                , site
                , FE.threadView website board_pathpart board_thread_id model
                )

            where
                model = FE.Model
                    { FE.grid_model = undefined
                    , FE.client_model = undefined
                    , FE.thread_model = Just thread_model
                    , FE.current_uri = undefined
                    , FE.media_root_ = undefined
                    , FE.current_time = t
                    , FE.tc_model = undefined
                    , FE.search_model = undefined
                    }

                thread_model = Thread.Model
                    { Thread.site = site
                    , Thread.media_root = pack $ media_root settings
                    , Thread.post_bodies = posts_and_bodies
                    , Thread.current_time = t
                    }

searchView :: Maybe Text -> Handler (HtmlPage (View FE.Action))
searchView _ = throwError $ err404 { errBody = "404 - Not Implemented" }

app :: JSONSettings -> Wai.Application
app settings =
    serve
        (Proxy @API)
        (staticHandler :<|> handlers settings)

    where
        staticHandler :: Server StaticRoute
        staticHandler = Servant.serveDirectoryFileServer (static_serve_path settings)

port :: Int
port = 8888

newtype CliArgs = CliArgs
  { settingsFile :: String
  } deriving (Show, Data, Typeable)

getSettings :: IO JSONSettings
getSettings = do
    cliArgs <- cmdArgs $ CliArgs "settings.json"

    let filePath = settingsFile cliArgs
    if null filePath
    then do
        putStrLn "Error: No JSON settings file provided."
        exitFailure
    else do
        putStrLn $ "Loading settings from: " ++ filePath
        content <- B.readFile filePath
        case decode content :: Maybe JSONSettings of
            Nothing -> do
                putStrLn "Error: Invalid JSON format."
                exitFailure
            Just settings -> return settings

main :: IO ()
main = do
    settings <- getSettings
    print settings

    putStrLn $ "Serving front-end on port " ++ show port

    Wai.run port $ Wai.logStdout (app settings)
