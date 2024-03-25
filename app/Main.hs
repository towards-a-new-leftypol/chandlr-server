{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import System.Exit (exitFailure)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack)
import Miso (ToServerRoutes, View)
import Miso.String (toMisoString)
import           Data.Proxy
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp             as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai
import Servant.API
import Servant.Server (Server, Handler (..), serve, err500, ServerError (..))
import qualified Lucid      as L
-- import qualified Lucid.Base as L
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.UTF8 (fromString)
import System.Console.CmdArgs (cmdArgs, Data, Typeable)
import Data.Aeson (decode)
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

newtype HtmlPage a = HtmlPage (FE.Model, a)

instance (L.ToHtml a) => L.ToHtml (HtmlPage a) where
    toHtmlRaw = L.toHtml
    toHtml (HtmlPage (_, x)) = L.toHtml x
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

handlers :: JSONSettings -> Server FrontEndRoutes
handlers settings = (catalogView settings) :<|> threadView :<|> searchView

clientSettings :: JSONSettings -> S.JSONSettings
clientSettings (JSONSettings {..}) = S.JSONSettings
    { S.postgrest_url = postgrest_url
    , S.jwt = jwt
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
        render t posts = HtmlPage (model, FE.catalogView model)
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

threadView :: Text -> Text -> FE.BoardThreadId -> Handler (HtmlPage (View FE.Action))
threadView = undefined

searchView :: Maybe Text -> Handler (HtmlPage (View FE.Action))
searchView = undefined

app :: JSONSettings -> Wai.Application
app settings = serve (Proxy @FrontEndRoutes) (handlers settings)

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
