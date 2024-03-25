module DataClient
    ( fetchLatest
    )
    where

import Data.Time.Clock (UTCTime)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LC8

import Common.Network.CatalogPostType (CatalogPost)
import Common.Network.ClientTypes (Model (..), FetchCatalogArgs (..))
import Common.Network.HttpClient
    ( post
    , HttpError (..)
    )
import Data.Aeson (eitherDecode, encode, FromJSON)
import Common.Server.JSONSettings (JSONSettings)

fetchLatest :: JSONSettings -> Model -> UTCTime -> IO (Either HttpError [ CatalogPost ])
fetchLatest settings m t = do
    post settings "/rpc/fetch_catalog" payload False >>= return . eitherDecodeResponse

    where
        payload = encode FetchCatalogArgs
            { max_time = t
            , max_row_read = fetchCount m
            }

eitherDecodeResponse :: (FromJSON a) => Either HttpError LBS.ByteString -> Either HttpError a
eitherDecodeResponse (Left err) = Left err
eitherDecodeResponse (Right bs) =
    case eitherDecode bs of
        Right val -> Right val
        Left err -> Left $ StatusCodeError 500 $ LC8.pack $ "Failed to decode JSON: " ++ err ++ " " ++ (show bs)
