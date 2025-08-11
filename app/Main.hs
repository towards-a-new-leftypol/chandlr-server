{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Main where

import System.Exit (exitFailure)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack)
import Data.Text.Lazy (toStrict)
import Miso
    ( View
    , HTML
    , ToHtml (..)
    , doctype_
    , html_
    , head_
    , meta_
    , charset_
    , name_
    , content_
    , rel_
    , link_
    , href_
    , src_
    , body_
    , type_
    , script_
    , class_
    , ToView (..)
    )
import Miso.Html.Element (title_)
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
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.UTF8 (fromString)
import System.Console.CmdArgs (cmdArgs, Data, Typeable)
import Data.Aeson (decode, ToJSON)
import Data.Aeson.Text (encodeToLazyText)
import Data.Time.Clock (getCurrentTime, UTCTime)
import Control.Monad.Except (throwError)

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
import Common.Network.ClientTypes (GetThreadArgs (..))
import qualified Common.Component.Thread.Model as Thread
import qualified Common.Component.Thread as Thread
import Common.Network.SiteType (Site)

data IndexPage a = forall b. (ToJSON b) => IndexPage (JSONSettings, b, a)

instance (ToView a) => ToHtml (IndexPage a) where
    toHtml (IndexPage (settings, initial_data, x)) = toHtml
        [ doctype_
        , html_
            []
            [ head_
                []
                [ meta_ [ charset_ "utf-8" ]
                , meta_
                    [ name_ "viewport"
                    , content_ "width=device-width, initial-scale=1.0"
                    ]
                , embedData "postgrest-root" (toMisoString $ postgrest_url settings)
                , embedData "postgrest-fetch-count" (toMisoString $ postgrest_fetch_count settings)
                , embedData "media-root" (toMisoString $ media_root settings)
                -- , embedData "initial-data" (toStrict $ encodeToLazyText initial_data)
                , script_
                    [ class_ "initial-data"
                    , type_ "application/json"
                    ]
                    (toStrict $ encodeToLazyText initial_data)

                , title_ [] [ "Chandlr" ]

                -- , js $ static_root <> "/static/init.js"
                , css $ static_root <> "/static/style.css"
                ]
            , body_ [] [ toView x ]
            ]
        ]

        where
            embedData name value = meta_ [ name_ name, content_ value ]

            static_root = static_serve_url_root settings

            css href =
                link_
                    [ rel_ "stylesheet"
                    , type_ "text/css"
                    , href_ $ toMisoString href
                    ]

            js href =
                script_
                    [ type_ "module"
                    , src_ $ toMisoString href
                    ]
                    ""

{-
    :Created By:
      ___________________________              _____    _______________________
     /\________________________  \            /\  __`\ /\______________________\
     \/_______________________//'/   __   _ __\ \ \/\ \\/______________________/
                             //'/' /'__`\/\`'__\ \ \ \ \
                            //'/' /\  __/\ \ \/ \ \ \_\ \
                           //'/'  \ \____\\ \_\  \ \_____\
                          //_/'____\/____/_\/_/___\/_____/___
                         /\__________________________________\
                         \/__________________________________/

-}



type StaticRoute = "static" :> Servant.Raw

type ServerRoutes = FE.Route (Get '[HTML] (IndexPage (View FE.Action)))

type API = StaticRoute :<|> ServerRoutes

handlers :: JSONSettings -> Server ServerRoutes
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

catalogView :: JSONSettings -> Handler (IndexPage (View FE.Action))
catalogView settings = do
    now <- liftIO getCurrentTime

    catalog_results <- liftIO $ do
        Client.fetchLatest
            (clientSettings settings)
            (clientModel settings)
            now

    case catalog_results of
        Left err -> throwError $ err500 { errBody = fromString $ show err }
        Right posts -> pure $ render now posts

    where
        render :: UTCTime -> [ CatalogPost ] -> IndexPage (View FE.Action)
        render t posts =
            IndexPage
                ( settings
                , posts
                , FE.catalogView tc grid model
                )

            where
                m_root = toMisoString $ media_root settings

                model = FE.Model
                    { FE.current_uri = undefined
                    , FE.media_root_ = m_root
                    , FE.current_time = t
                    , search_term = ""
                    , initial_action = FE.NoAction
                    , thread_message = Nothing
                    , pg_api_root = toMisoString $ postgrest_url settings
                    , client_fetch_count = 100
                    , my_component_id = undefined
                    }

                grid_model = Grid.Model
                    { Grid.media_root = m_root
                    , Grid.display_items = posts
                    }

                grid = Grid.mkApp grid_model

                tc = TC.app 0


threadView :: JSONSettings -> Text -> Text -> FE.BoardThreadId -> Handler (IndexPage (View FE.Action))
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
        render :: [ Thread.PostWithBody ] -> UTCTime -> Site -> IndexPage (View FE.Action)
        render posts_and_bodies t site =
            IndexPage
                ( settings
                , site
                , FE.mkThreadView
                    thread_model
                    website
                    board_pathpart
                    board_thread_id
                    model
                )

            where
                model = FE.Model
                    { FE.current_uri = undefined
                    , FE.media_root_ = undefined
                    , FE.current_time = t
                    , search_term = ""
                    , initial_action = FE.NoAction
                    , thread_message = Nothing
                    , pg_api_root = toMisoString $ postgrest_url settings
                    , client_fetch_count = 100
                    , my_component_id = undefined
                    }

                thread_model = Thread.Model
                    { Thread.site = site
                    , Thread.media_root = pack $ media_root settings
                    , Thread.post_bodies = posts_and_bodies
                    , Thread.current_time = t
                    }


searchView :: Maybe Text -> Handler (IndexPage (View FE.Action))
searchView _ = throwError $ err404 { errBody = "404 - Not Implemented" }

server :: JSONSettings -> Wai.Application
server settings =
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

    Wai.run port $ Wai.logStdout (server settings)
