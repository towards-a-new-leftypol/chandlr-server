{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- Note: Changed to `Main` for `runhaskell` compatibility.
-- If you prefer to keep your module name, compile with `ghc` instead.
module Main where

import GHC.Generics
import qualified Miso.JSON as Miso
import Miso.String (MisoString, fromMisoString, toMisoString)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Data.Time.LocalTime (ZonedTime, zonedTimeToUTC)
import Data.Time.Format.ISO8601 (iso8601Show, iso8601ParseM)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Control.DeepSeq (force)
import Data.Text.Encoding (decodeUtf8)

data CatalogPost = CatalogPost
    { post_id              :: Maybe Integer
    , board_post_id        :: Integer
    , board_thread_id      :: Integer
    -- , creation_time        :: UTCTime
    -- , bump_time            :: UTCTime
    , body                 :: Maybe MisoString
    , name                 :: Maybe MisoString
    , subject              :: Maybe MisoString
    , email                :: Maybe MisoString
    , thread_id            :: Int
    -- , post_count           :: Int
    , embed                :: Maybe MisoString
    , estimated_post_count :: Int
    , site_name            :: MisoString
    , pathpart             :: MisoString
    , site_id              :: Int
    , file_mimetype        :: Maybe MisoString
    , file_illegal         :: Maybe Bool
    , file_resolution      :: Maybe Dimension
    , file_name            :: Maybe MisoString
    , file_extension       :: Maybe MisoString
    , file_thumb_extension :: Maybe MisoString
    }
    deriving (Generic, Miso.FromJSON, Miso.ToJSON, Aeson.FromJSON, Aeson.ToJSON)

data Dimension = Dimension
  { width  :: Int
  , height :: Int
  } deriving (Generic, Miso.FromJSON, Miso.ToJSON, Aeson.FromJSON, Aeson.ToJSON)

-- Miso instances for UTCTime (kept as you wrote them)
utcToIso :: UTCTime -> MisoString
utcToIso = toMisoString . iso8601Show

isoToUtc :: MisoString -> Miso.Parser UTCTime
isoToUtc t = zonedTimeToUTC <$> (iso8601ParseM (fromMisoString t) :: Miso.Parser ZonedTime)
-- This code fails on "2024-10-30T22:46:38Z"


instance Miso.ToJSON UTCTime where
    toJSON = Miso.String . utcToIso

instance Miso.FromJSON UTCTime where
    parseJSON (Miso.String x) = isoToUtc x
    parseJSON _ = fail "Expected String for UTCTime"


countPosts :: [ CatalogPost ] -> Int
countPosts = sum . (map estimated_post_count)

jsonFilePath :: String
jsonFilePath = "catalog_post_testdata2.json"

main :: IO ()
main = do
    lbs <- LBS.readFile jsonFilePath

    fileAsText <- decodeUtf8 <$> BS.readFile jsonFilePath

    print fileAsText

    putStrLn "=== Benchmarking Miso.JSON ==="
    timeBench "Miso.JSON decode" $ do
        case ((Miso.eitherDecode fileAsText) :: Either MisoString [ CatalogPost ]) of
            Left err -> error $ fromMisoString $ "Miso.JSON decode failed" <> err
            Right posts -> pure $ countPosts posts

    putStrLn "=== Benchmarking Aeson ==="
    timeBench "Aeson decode" $ do
        case Aeson.eitherDecode lbs of
            Left err -> error $ "Aeson decode failed: " ++ err
            Right posts -> pure $ countPosts posts

-- Timing helper that ensures full strict evaluation before stopping the clock
timeBench :: String -> IO Int -> IO ()
timeBench label action = do
    start <- getCurrentTime
    !result <- action
    end <- getCurrentTime
    let !forced = force result -- Force full evaluation of the sum
    putStrLn $ "Sum of estimated_post_count: " ++ show forced
    putStrLn $ label ++ " took: " ++ show (diffUTCTime end start)
