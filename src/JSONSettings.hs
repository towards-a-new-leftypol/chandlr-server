module JSONSettings where

import GHC.Generics
import Data.Aeson (FromJSON)

data JSONSettings = JSONSettings
    { postgrest_url :: String
    , jwt :: String
    , postgrest_fetch_count :: Int
    , media_root :: String
    , static_serve_path :: String
    , static_serve_url_root :: String
    } deriving (Show, Generic)

instance FromJSON JSONSettings
