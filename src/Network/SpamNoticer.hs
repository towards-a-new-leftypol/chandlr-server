{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module Network.SpamNoticer where

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON, encode)
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Network.HTTP.Simple
    ( setRequestMethod
    , parseRequest
    , httpLBS
    )
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Data.List.NonEmpty as L
import Miso (MisoString)

import Common.Network.HttpClient (HttpError, handleHttp)
import Network.HTTP.Client.MultipartFormData
    ( formDataBody
    , partLBS
    , partFileSource
    )
import Hash (computeMD5)

import qualified Common.FrontEnd.JSONSettings as Settings
import Network.DataClient (eitherDecodeResponse)
import qualified Common.Network.SiteType  as Site
import qualified Common.Network.BoardType as Board
import qualified Common.Network.ThreadType as Thread
import qualified Common.Network.PostType  as Post
import qualified Common.AttachmentType    as Att


-- | Metadata for a single attachment in the spam report.
data SpamNoticerAttachmentMetadata = SpamNoticerAttachmentMetadata
    { filename   :: Text
    , mimetype   :: Text
    , md5_hash   :: Text
    , is_spam    :: Bool
    , is_illegal :: Bool
    } deriving (Show, Generic, ToJSON)

-- | The JSON payload sent as the "json" multipart part.
data SpamNoticerAddRequestInfo = SpamNoticerAddRequestInfo
    { attachments   :: [ SpamNoticerAttachmentMetadata ]
    , body          :: Maybe Text
    , body_is_spam  :: Bool
    , time_stamp    :: Integer
    , website_name  :: Text
    , board_name    :: Text
    , thread_id     :: Integer
    , reporter_name :: Text
    } deriving (Show, Generic, ToJSON)

data SpamNoticerResponse = SpamNoticerResponse
    { result :: MisoString
    } deriving (Show, Generic, FromJSON)

addToKnownSpam
    :: Settings.JSONSettings
    -> Maybe Text
    -> Site.Site
    -> [ FilePath ]
    -> IO (Either HttpError SpamNoticerResponse)
addToKnownSpam settings reporterName site attachmentPaths = do
    putStrLn $ "POSTing to SpamNoticer /add_post_to_known_spam: " ++ url
    requestInfo <- buildRequestInfo reporterName site attachmentPaths

    eitherDecodeResponse <$>
      ( handleHttp $ do
            req <- parseRequest url
            let httpRequest = setRequestMethod "POST" req
            request <- formDataBody (jsonPart requestInfo : attachmentParts) httpRequest
            httpLBS request
      )

    where
        url = Settings.spam_noticer_url settings ++ "/add_post_to_known_spam"

        jsonPart ri = partLBS "json" $ encode ri

        attachmentParts = map (partFileSource "attachments") attachmentPaths


buildRequestInfo
    :: Maybe Text
    -> Site.Site
    -> [ FilePath ]
    -> IO SpamNoticerAddRequestInfo
buildRequestInfo reporterName site attachmentPaths = do
    hashes <- mapM computeMD5 attachmentPaths

    return SpamNoticerAddRequestInfo
        { attachments   = zipWith makeAttachmentMeta (Post.attachments post) hashes
        , body          = Post.body post
        , body_is_spam  = null attachments
        , time_stamp    = round $ utcTimeToPOSIXSeconds $ Post.creation_time post
        , website_name  = Site.name site
        , board_name    = Board.pathpart board
        , thread_id     = Thread.board_thread_id thread
        , reporter_name = fromMaybe "chan_archive_admin" reporterName
        }

    where
        board  = L.head (Site.boards site)
        thread = head (Board.threads board)
        post   = L.head (Thread.posts thread)
        attachments = Post.attachments post

        makeAttachmentMeta :: Att.Attachment -> Text -> SpamNoticerAttachmentMetadata
        makeAttachmentMeta att md5 = SpamNoticerAttachmentMetadata
            { filename   = Att.board_filename att
            , mimetype   = Att.mimetype att
            , md5_hash   = md5
            , is_spam    = True
            , is_illegal = True
            }
