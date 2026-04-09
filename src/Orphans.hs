-- stupid hack, but perhaps easier and cleaner than using CPP to conditionally add aeson instances everywhere

{-# LANGUAGE DeriveAnyClass #-}

module Orphans where

import Data.Aeson (FromJSON, ToJSON)
import Common.Network.ClientTypes (GetThreadArgs, SearchPostsArgs)
import Common.FrontEnd.Types
import Common.Network.CatalogPostType (CatalogPost)
import qualified Common.AttachmentType as Attachment
import qualified Common.Network.PostType as Post
import qualified Common.Network.ThreadType as Thread
import qualified Common.Network.BoardType as Board
import qualified Common.Network.SiteType as Site
import qualified Common.Network.ClientTypes as Client
import Common.FrontEnd.JSONSettings (JSONSettings)
import Common.Parsing.PostPartType (PostPart)
import Common.Parsing.QuoteLinkParser (ParsedURL)
import Common.Parsing.FlexibleJsonResponseParser as Flx

instance FromJSON Client.DeleteIllegalPostArgs
instance FromJSON JSONSettings
instance FromJSON CatalogPost
instance FromJSON Attachment.Dimension
instance FromJSON Site.Site
instance FromJSON Board.Board
instance FromJSON Thread.Thread
instance FromJSON Post.Post
instance FromJSON Attachment.Attachment
instance FromJSON Flx.SSite

instance ToJSON Site.Site
instance ToJSON Board.Board
instance ToJSON Thread.Thread
instance ToJSON Post.Post
instance ToJSON Attachment.Dimension
instance ToJSON Attachment.Attachment
instance ToJSON CatalogPost
instance ToJSON InitialData
instance ToJSON InitialDataPayload
instance ToJSON SearchPostsArgs
instance ToJSON PostPart
instance ToJSON ParsedURL
instance ToJSON Client.FetchCatalogArgs
