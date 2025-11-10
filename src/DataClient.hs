{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}

module DataClient
    ( fetchLatest
    , getThread
    , search
    , getPost
    )
    where

import Data.Time.Clock (UTCTime)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.Aeson (eitherDecode, encode, FromJSON)
import Miso.String (fromMisoString, MisoString)

import Common.Network.CatalogPostType (CatalogPost)
import Common.Network.ClientTypes
import Common.Network.HttpClient
    ( post
    , get
    , HttpError (..)
    )
import Common.Server.JSONSettings (JSONSettings)
import Common.Network.SiteType (Site)
import Common.Parsing.FlexibleJsonResponseParser as Flx


fetchLatest :: JSONSettings -> Model -> UTCTime -> IO (Either HttpError [ CatalogPost ])
fetchLatest settings m t = do
    post settings "/rpc/fetch_catalog" payload False >>= return . eitherDecodeResponse

    where
        payload = encode FetchCatalogArgs
            { max_time = t
            , max_row_read = fetchCount m
            }


getThread :: JSONSettings -> GetThreadArgs -> IO (Either HttpError [ Site ])
getThread settings GetThreadArgs {..} = do
    get settings path >>= return . eitherDecodeResponse

    where
        path = "/sites?"
            <> "select=*,boards(*,threads(*,posts(*,attachments(*))))"
            <> "&name=eq." <> fromMisoString website
            <> "&boards.pathpart=eq." <> fromMisoString board_pathpart
            <> "&boards.threads.board_thread_id=eq." <> show board_thread_id
            <> "&boards.threads.posts.order=board_post_id.asc"


getPost :: JSONSettings -> Integer -> IO (Either HttpError [ Site ])
getPost settings post_id = do
    response <- get settings path
    return $ sitesFromSSites <$> eitherDecodeResponse response

    where
        path = "/sites?"
            <> "select=*,boards!inner(*,threads!inner(*,posts!inner(*,attachments(*))))"
            <> "&boards.threads.posts.post_id=eq." <> show post_id


search :: JSONSettings -> MisoString -> IO (Either HttpError [ CatalogPost ])
search settings query = do
    post settings "/rpc/search_posts" payload False >>= return . eitherDecodeResponse

    where
        payload = encode SearchPostsArgs
            { search_text = query
            , max_rows = 200
            }


eitherDecodeResponse :: (FromJSON a) => Either HttpError LBS.ByteString -> Either HttpError a
eitherDecodeResponse (Left err) = Left err
eitherDecodeResponse (Right bs) =
    case eitherDecode bs of
        Right val -> Right val
        Left err -> Left $ StatusCodeError 500 $ LC8.pack $ "Failed to decode JSON: " ++ err ++ " " ++ show bs


siteFromSSite :: Flx.SSite -> Site
siteFromSSite (SSite s) = s


sitesFromSSites :: [ Flx.SSite ] -> [ Site ]
sitesFromSSites = map siteFromSSite
