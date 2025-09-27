{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Main where

import System.Exit (exitFailure)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack)
import Miso (View)
import qualified Miso as M
import Miso.String (toMisoString, fromMisoString)
import Miso.Html (toHtml)
import Miso.Router (parseURI)
import Servant.Miso.Html (HTML)
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
import Data.Aeson (decode)
import Data.Time.Clock (getCurrentTime, UTCTime)
import Control.Monad.Except (throwError)
import Data.Either (fromRight)

import Common.FrontEnd.JSONSettings
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
import Common.Network.SiteType (Site)
import Common.Component.BodyRender (getPostWithBodies)
import qualified Common.Component.BodyRender as Body
import IndexPage (IndexPage (..))
import qualified Common.FrontEnd.MainComponent as MC

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

type PageType = IndexPage MC.MainComponent

type GET_Result = Get '[HTML] PageType

type ServerRoutes = FE.Route GET_Result

type API = StaticRoute :<|> ServerRoutes


handlers :: JSONSettings -> Server ServerRoutes
handlers settings
    =    (catalogView settings)
    :<|> (threadView settings)
    :<|> searchView


server :: JSONSettings -> Wai.Application
server settings =
    serve
        (Proxy @API)
        (staticHandler :<|> handlers settings)

    where
        staticHandler :: Server StaticRoute
        staticHandler = Servant.serveDirectoryFileServer (static_serve_path settings)


clientSettings :: JSONSettings -> S.JSONSettings
clientSettings (JSONSettings {..}) = S.JSONSettings
    { S.postgrest_url = fromMisoString postgrest_url
    , S.jwt = pack jwt
    , S.backup_read_root = undefined
    , S.media_root_path = undefined
    , S.site_name = undefined
    , S.site_url = undefined
    }


clientModel :: JSONSettings -> Client.Model
clientModel (JSONSettings {..}) = Client.Model
    { Client.pgApiRoot = toMisoString postgrest_url
    , Client.fetchCount = postgrest_fetch_count
    }


serverRouteLink
    :: forall endpoint.
       ( IsElem endpoint ServerRoutes
       , HasLink endpoint
       )
    => Proxy endpoint
    -> MkLink endpoint Link
serverRouteLink = safeLink (Proxy @ServerRoutes)


routeLinkToURI :: Link -> M.URI
routeLinkToURI = (fromRight (M.URI mempty mempty mempty)) . parseURI . toMisoString . toUrlPiece


-- Wat! Why doesn't this build but the usage below does !?
-- endpointUrl :: _
-- endpointUrl = routeLinkToURI . serverRouteLink


catalogView :: JSONSettings -> Handler PageType
catalogView settings = do
    now <- liftIO getCurrentTime

    catalog_results <- liftIO $ do
        Client.fetchLatest
            (clientSettings settings)
            (clientModel settings)
            now

    case catalog_results of
        Left err -> throwError $ err500 { errBody = fromString $ show err }
        Right posts -> pure $
            let initialData = MC.CatalogData posts
                initialDataPayload = MC.InitialDataPayload now initialData
            in

            IndexPage
                ( settings
                , posts
                , MC.app settings uri initialDataPayload
                )

    where
        uri = (routeLinkToURI . serverRouteLink) (Proxy :: Proxy GET_Result)


threadView :: JSONSettings -> Text -> Text -> FE.BoardThreadId -> Handler PageType
threadView settings website board_pathpart board_thread_id = do
    thread_results <- liftIO $ do

        Client.getThread
            (clientSettings settings)
            (clientModel settings)
            (GetThreadArgs
                { website = toMisoString website
                , board_pathpart = toMisoString board_pathpart
                , board_thread_id = board_thread_id
                }
            )

    now <- liftIO getCurrentTime

    case thread_results of
        Left err -> throwError $ err500 { errBody = fromString $ show err }
        Right site -> do
            let
                s = head site
                posts_and_bodies = getPostWithBodies s
            liftIO $ do
                putStrLn "threadView - posts and bodies:"
                let first_post_parts = snd $ head posts_and_bodies
                print first_post_parts
                print $ toHtml $ Body.render s first_post_parts
            pure $ render posts_and_bodies now s

    where
        render :: [ Thread.PostWithBody ] -> UTCTime -> Site -> IndexPage (View FE.Model FE.Action)
        render posts_and_bodies t site =
            IndexPage
                ( settings
                , site
                , FE.threadView
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
                    }

                thread_model = Thread.Model
                    { Thread.site = site
                    , Thread.media_root = toMisoString $ media_root settings
                    , Thread.post_bodies = posts_and_bodies
                    , Thread.current_time = t
                    }


searchView :: Maybe String -> Handler PageType
searchView _ = throwError $ err404 { errBody = "404 - Not Implemented" }


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
