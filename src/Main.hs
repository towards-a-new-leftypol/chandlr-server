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
import Data.Proxy
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
    , err403
    , ServerError (..)
    )
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.UTF8 (fromString)
import System.Console.CmdArgs (cmdArgs, Data, Typeable)
import Data.Aeson (decode)
import Data.Time.Clock (getCurrentTime)
import Control.Monad.Except (throwError)
import Data.Either (fromRight)
import Data.IORef (newIORef)
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Control.Concurrent.Async (concurrently)

import Common.FrontEnd.JSONSettings
import qualified Common.FrontEnd.Routes as FE
import qualified DataClient as Client
import qualified Common.Network.ClientTypes as Client
import qualified Common.Network.HttpClient as Client
import qualified Common.Server.JSONSettings as S
import Common.Network.ClientTypes (GetThreadArgs (..))
import Common.Component.BodyRender (getPostWithBodies)
import IndexPage (IndexPage (..))
import Common.FrontEnd.MainComponent (app)
import Common.FrontEnd.Types
import Admin.DeletePostHandler (deletePostHandler)
import qualified Common.Network.SiteType as Site

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

type PageType = IndexPage

type GET_Result = Get '[HTML] PageType

type AdminApi
    =  "admin_"
    :> "delete_post"
    :> ReqBody '[JSON] Client.DeleteIllegalPostArgs
    :> Post '[JSON] [ Site.Site ]

type ServerRoutes
    = FE.Route GET_Result
    :<|> AdminApi

type API = StaticRoute :<|> ServerRoutes


handlers :: JSONSettings -> Server ServerRoutes
handlers settings
    =
    (        (catalogView settings)
        :<|> (threadView settings)
        :<|> (searchView settings)
    )
    :<|> deleteHandler

    where
        deleteHandler
            | admin settings = deletePostHandler (clientSettings settings)
            | otherwise = const $ throwError err403
            -- this is a shitty attempt at security tbh
            -- the idea is that you launch one server with admin false and
            -- route that to users, and you launch another instance that's
            -- behind an authenticating proxy for mods.


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
    , S.media_root_path = fromMisoString media_root_path
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


catalogView :: JSONSettings -> Maybe String -> Handler PageType
catalogView settings t = do
    obsrvrTime <- case t of
        Nothing -> liftIO getCurrentTime
        Just time -> return $ read time

    catalogResults <-
            let servSettings = clientSettings settings
            in liftIO $ loadSitesAndBoardsAndData servSettings $
                Client.fetchLatest
                    servSettings
                    (clientModel settings)
                    obsrvrTime

    case catalogResults of
        Left err -> throwError $ err500 { errBody = fromString $ show err }
        Right (posts, sites) -> do
            let initialData = CatalogData posts
            let initialDataPayload = InitialDataPayload obsrvrTime initialData sites
            let ctx = AppInitCtx True uri settings initialDataPayload

            ctxRef <- liftIO $ newIORef ctx

            pure $
                IndexPage
                    ( settings
                    , initialDataPayload
                    , app ctxRef
                    )

    where
        proxy :: Proxy (FE.R_Latest GET_Result)
        proxy = Proxy

        uri :: M.URI
        uri = routeLinkToURI $ serverRouteLink proxy t


threadView :: JSONSettings -> Text -> Text -> FE.BoardThreadId -> Handler PageType
threadView settings website board_pathpart board_thread_id = do
    threadResults <-
            let servSettings = clientSettings settings
            in liftIO $ loadSitesAndBoardsAndData servSettings $
                Client.getThread
                    servSettings
                    (GetThreadArgs
                        { website = toMisoString website
                        , board_pathpart = toMisoString board_pathpart
                        , board_thread_id = board_thread_id
                        }
                    )

    now <- liftIO getCurrentTime

    case threadResults of
        Left err -> throwError $ err500 { errBody = fromString $ show err }
        Right (site, sites) -> do
            let s                  = head site
            let postsWithBodies    = getPostWithBodies s
            let threadData         = ThreadData s postsWithBodies
            let initialDataPayload = InitialDataPayload now threadData sites
            let ctx                = AppInitCtx True uri settings initialDataPayload

            ctxRef <- liftIO $ newIORef ctx

            pure $ IndexPage
                ( settings
                , initialDataPayload
                , app ctxRef
                )

    where
        proxy = (Proxy :: Proxy (FE.R_Thread GET_Result))

        uri :: M.URI
        -- this is crazy that this works:
        uri = routeLinkToURI $
                serverRouteLink proxy website board_pathpart board_thread_id


searchView :: JSONSettings -> Maybe String -> Handler PageType
searchView settings Nothing = do
    liftIO $ putStrLn "Main - inside searchView - DO NOT HAVE queryParam"
    now <- liftIO getCurrentTime

    sitesResult <- liftIO $
        Client.getAllSitesAndBoards (clientSettings settings)

    sites <- case sitesResult of
        Left err -> do
            liftIO $ putStrLn $ "Loading Sites and Boards failed: " ++ show err
            return fakeSitesForError
        Right s -> return s

    let initialDataPayload = InitialDataPayload now (SearchData []) sites
    let ctx = AppInitCtx True uri settings initialDataPayload

    ctxRef <- liftIO $ newIORef ctx

    pure $ IndexPage
        ( settings
        , initialDataPayload
        , app ctxRef
        )

    where
        proxy = (Proxy :: Proxy (FE.R_SearchResults GET_Result))

        uri :: M.URI
        uri = routeLinkToURI $ serverRouteLink proxy Nothing

searchView settings queryParam@(Just query) = do
    liftIO $ putStrLn "Main - inside searchView - have queryParam"
    now <- liftIO getCurrentTime

    searchResult <-
            let servSettings = clientSettings settings
            in liftIO $ loadSitesAndBoardsAndData servSettings $
                Client.search
                    servSettings
                    query

    case searchResult of
        Left err -> throwError $ err500 { errBody = fromString $ show err }
        Right (posts, sites) -> do
            liftIO $ putStrLn $ "searchView - number of search results: " ++ show (length posts)
            let initialData = SearchData posts
            let initialDataPayload = InitialDataPayload now initialData sites
            let ctx = AppInitCtx True uri settings $ initialDataPayload

            ctxRef <- liftIO $ newIORef ctx

            pure $
                IndexPage
                    ( settings
                    , initialDataPayload
                    , app ctxRef
                    )

    where
        proxy = (Proxy :: Proxy (FE.R_SearchResults GET_Result))

        uri :: M.URI
        uri = routeLinkToURI $ serverRouteLink proxy queryParam


fakeSitesForError :: [ Site.Site ]
fakeSitesForError = Site.emptySite { Site.name = "ERROR LOADING SITES" } : []


loadSitesAndBoardsAndData
    :: S.JSONSettings
    -> IO (Either Client.HttpError a)
    -> IO (Either Client.HttpError (a, [ Site.Site ]))
loadSitesAndBoardsAndData settings otherHttpClientAction = do
    (a, b) <- concurrently otherHttpClientAction $ do
        Client.getAllSitesAndBoards settings

    case b of
        Left err -> do
            putStrLn $ "Loading Sites and Boards failed: " ++ show err
            return $ withSites a fakeSitesForError

        Right sites -> return $ withSites a sites

    where
        withSites :: Either a b -> [ Site.Site ] -> Either a (b, [ Site.Site ])
        withSites a sites = fmap (,sites) a


defaultPort :: Int
defaultPort = 8888


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

    portStr <- lookupEnv "PORT"
    let port = fromMaybe defaultPort (readMaybe =<< portStr)

    putStrLn $ "Serving on " ++ show port

    Wai.run port $ Wai.logStdout (server settings)
