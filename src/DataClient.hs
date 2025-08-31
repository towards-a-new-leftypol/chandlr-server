{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}

module DataClient
    ( fetchLatest
    , getThread
    )
    where

import Data.Time.Clock (UTCTime)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.Aeson (eitherDecode, encode, FromJSON)
import Miso.String (fromMisoString)

import Common.Network.CatalogPostType (CatalogPost)
import Common.Network.ClientTypes (Model (..), FetchCatalogArgs (..), GetThreadArgs (..))
import Common.Network.HttpClient
    ( post
    , get
    , HttpError (..)
    )
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

getThread :: JSONSettings -> Model -> GetThreadArgs -> IO (Either HttpError [ Site ])
getThread settings _ GetThreadArgs {..} =
    get settings path >>= return . eitherDecodeResponse

    where
        path = "/sites?"
            <> "select=*,boards(*,threads(*,posts(*,attachments(*))))"
            <> "&name=eq." <> fromMisoString website
            <> "&boards.pathpart=eq." <> fromMisoString board_pathpart
            <> "&boards.threads.board_thread_id=eq." <> show board_thread_id
            <> "&boards.threads.posts.order=board_post_id.asc"

eitherDecodeResponse :: (FromJSON a) => Either HttpError LBS.ByteString -> Either HttpError a
eitherDecodeResponse (Left err) = Left err
eitherDecodeResponse (Right bs) =
    case eitherDecode bs of
        Right val -> Right val
        Left err -> Left $ StatusCodeError 500 $ LC8.pack $ "Failed to decode JSON: " ++ err ++ " " ++ show bs
