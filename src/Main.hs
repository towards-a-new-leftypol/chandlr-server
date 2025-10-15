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
import qualified Miso as M
import Miso.String (toMisoString, fromMisoString)
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
    , ServerError (..)
    )
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.UTF8 (fromString)
import System.Console.CmdArgs (cmdArgs, Data, Typeable)
import Data.Aeson (decode)
import Data.Time.Clock (getCurrentTime)
import Control.Monad.Except (throwError)
import Data.Either (fromRight)

import Common.FrontEnd.JSONSettings
import qualified Common.FrontEnd.Routes as FE
import qualified DataClient as Client
import qualified Common.Network.ClientTypes as Client
import qualified Common.Server.JSONSettings as S
import Common.Network.ClientTypes (GetThreadArgs (..))
import Common.Component.BodyRender (getPostWithBodies)
import IndexPage (IndexPage (..))
import Common.FrontEnd.MainComponent (MainComponent, app)
import Common.FrontEnd.Types

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

type PageType = IndexPage MainComponent

type GET_Result = Get '[HTML] PageType

type ServerRoutes = FE.Route GET_Result

type API = StaticRoute :<|> ServerRoutes


handlers :: JSONSettings -> Server ServerRoutes
handlers settings
    =    (catalogView settings)
    :<|> (threadView settings)
    :<|> (searchView settings)


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
routeLinkToURI = (fromRight (M.URI mempty mempty mempty)) . parseURI . ("/" <>) . toMisoString . toUrlPiece


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
            let initialData = CatalogData posts
                initialDataPayload = InitialDataPayload now initialData
            in

            IndexPage
                ( settings
                , now
                , initialDataPayload
                , app settings uri initialDataPayload
                )

    where
        uri :: M.URI
        uri = (routeLinkToURI . serverRouteLink) (Proxy :: Proxy (FE.R_Latest GET_Result))


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
        Right site -> pure $
            let
                s                  = head site
                postsWithBodies    = getPostWithBodies s
                threadData         = ThreadData s postsWithBodies
                initialDataPayload = InitialDataPayload now threadData
            in

            IndexPage
                ( settings
                , now
                , initialDataPayload
                , app settings uri initialDataPayload
                )

    where
        proxy = (Proxy :: Proxy (FE.R_Thread GET_Result))

        uri :: M.URI
        -- this is crazy that this works:
        uri = routeLinkToURI $
                serverRouteLink proxy website board_pathpart board_thread_id


searchView :: JSONSettings -> Maybe String -> Handler PageType
searchView settings Nothing = do
    now <- liftIO getCurrentTime

    let initialDataPayload = InitialDataPayload now (SearchData [])

    pure $ IndexPage
        ( settings
        , now
        , initialDataPayload
        , app settings uri initialDataPayload
        )

    where
        proxy = (Proxy :: Proxy (FE.R_SearchResults GET_Result))

        uri :: M.URI
        uri = routeLinkToURI $ serverRouteLink proxy Nothing

searchView settings queryParam@(Just query) = do
    now <- liftIO getCurrentTime

    searchResult <- liftIO $ do
        Client.search
            (clientSettings settings)
            (toMisoString query)

    case searchResult of
        Left err -> throwError $ err500 { errBody = fromString $ show err }
        Right posts -> pure $
            let initialData = SearchData posts
                initialDataPayload = InitialDataPayload now initialData
            in

            IndexPage
                ( settings
                , now
                , initialDataPayload
                , app settings uri initialDataPayload
                )

    where
        proxy = (Proxy :: Proxy (FE.R_SearchResults GET_Result))

        uri :: M.URI
        uri = routeLinkToURI $ serverRouteLink proxy queryParam


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
