module Parsing.BodyParser
    ( PostPart (..)
    , parsePostBody
    , collectBacklinks
    , Backlinks
    ) where

import Data.Text (Text)

import Common.Parsing.PostPartType
import Common.Parsing.PostBodyUtils

parsePostBody :: Text -> IO [ PostPart ]
parsePostBody = undefined
