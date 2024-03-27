{-# LANGUAGE RecordWildCards #-}

module DataClient
    ( fetchLatest
    , getThread
    )
    where

import Data.Time.Clock (UTCTime)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Text as Text

import Common.Network.CatalogPostType (CatalogPost)
import Common.Network.ClientTypes (Model (..), FetchCatalogArgs (..))
import Common.Network.HttpClient
    ( post
    , get
    , HttpError (..)
    )
import Data.Aeson (eitherDecode, encode, FromJSON)
import qualified Common.FrontEnd.Action as A
import Common.Server.JSONSettings (JSONSettings)
import Common.Network.SiteType (Site)

fetchLatest :: JSONSettings -> Model -> UTCTime -> IO (Either HttpError [ CatalogPost ])
fetchLatest settings m t = do
    post settings "/rpc/fetch_catalog" payload False >>= return . eitherDecodeResponse

    where
        payload = encode FetchCatalogArgs
            { max_time = t
            , max_row_read = fetchCount m
            }

getThread :: JSONSettings -> Model -> A.GetThreadArgs -> IO (Either HttpError [ Site ])
getThread settings m A.GetThreadArgs {..} =
    get settings path >>= return . eitherDecodeResponse

    where
        path = "/sites?"
            <> "select=*,boards(*,threads(*,posts(*,attachments(*))))"
            <> "&name=eq." <> Text.unpack website
            <> "&boards.pathpart=eq." <> Text.unpack board_pathpart
            <> "&boards.threads.board_thread_id=eq." <> (show board_thread_id)
            <> "&boards.threads.posts.order=board_post_id.asc"

eitherDecodeResponse :: (FromJSON a) => Either HttpError LBS.ByteString -> Either HttpError a
eitherDecodeResponse (Left err) = Left err
eitherDecodeResponse (Right bs) =
    case eitherDecode bs of
        Right val -> Right val
        Left err -> Left $ StatusCodeError 500 $ LC8.pack $ "Failed to decode JSON: " ++ err ++ " " ++ (show bs)
