module DataClient
( fetchLatest
)
where

import Common.Network.ClientTypes (Model (..))
import Common.Network.HttpClient
( post
, HttpError
)

fetchLatest :: Model -> UTCTime -> IO IO (Either HttpError [ CatalogPost ])
fetchLatest m t iface = do
    post settings "/rpc/fetch_catalog" payload False >>= return . eitherDecodeResponse

    where
        payload = encode FetchCatalogArgs
            { max_time = t
            , max_row_read = fetchCount m
            }
