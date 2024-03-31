{-# LANGUAGE OverloadedStrings #-}

module Parsing.BodyParser
    ( PostPart (..)
    , parsePostBody
    , collectBacklinks
    , Backlinks
    ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Text.HTML.Parser
    ( parseTokens
    , canonicalizeTokens
    , Token (..)
    , Attr(..)
    )
import Text.HTML.Tree (tokensToForest)
import Data.Tree (Forest, Tree (..))

import Common.Parsing.PostPartType
import Common.Parsing.QuoteLinkParser
import Common.Parsing.PostBodyUtils


getAttr :: Text -> [ Attr ] -> Maybe Text
getAttr _ [] = Nothing
getAttr attrName (Attr x y:xs)
    | x ==  attrName = Just y
    | otherwise = getAttr attrName xs


parsePostBody :: Text -> IO [ PostPart ]
parsePostBody html =
    case tokensToForest $ canonicalizeTokens $ parseTokens html of
        Left err -> do
            print err
            return []

        Right forest -> return $ forestToPostParts forest


forestToPostParts :: Forest Token -> [ PostPart ]
forestToPostParts = concatMap treeToPostParts


treeToPostParts :: Tree Token -> [ PostPart ]
treeToPostParts Node { rootLabel = (TagOpen "a" attrs) } =
    let m_href = getAttr "href" attrs
    in case m_href of
        Nothing ->
            [ SimpleText "Anchor without href" ]
        Just href ->
            let target = getAttr "target" attrs
            in case target of
                Just "_blank" ->
                    [ PostedUrl href ]
                _ ->
                    [ Quote $ parseURL $ Text.unpack href ]


treeToPostParts Node { rootLabel = (TagOpen "span" attrs), subForest } =
    maybe [] (:[]) $ foldr foldfunc Nothing classList

    where
        classList :: [ Text ]
        classList = maybe [] Text.words $ getAttr "class" attrs

        foldfunc :: Text -> Maybe PostPart -> Maybe PostPart
        foldfunc cls Nothing = (>>= \p -> Just $ p $ forestToPostParts subForest) $ matchPart cls
        foldfunc _ x@(Just _) = x

        matchPart :: Text -> Maybe ([ PostPart ] -> PostPart)
        matchPart "quote" = Just GreenText
        matchPart "orangeQuote" = Just OrangeText
        matchPart "heading" = Just RedText
        matchPart "spoiler" = Just Spoiler
        matchPart _ = Nothing

treeToPostParts Node { rootLabel = (TagOpen "em" _), subForest } =
    [ Italics $ forestToPostParts subForest ]

treeToPostParts Node { rootLabel = (TagOpen "strong" _), subForest } =
    [ Bold $ forestToPostParts subForest ]

treeToPostParts Node { rootLabel = (TagOpen "u" _), subForest } =
    [ Underlined $ forestToPostParts subForest ]

treeToPostParts Node { rootLabel = (TagOpen "s" _), subForest } =
    [ Strikethrough $ forestToPostParts subForest ]

treeToPostParts Node { rootLabel = (TagOpen "pre" _), subForest } =
    [ Code $ forestToPostParts subForest ]

treeToPostParts Node { rootLabel = (TagOpen "br" _) } =
    [ Skip ]

treeToPostParts Node { rootLabel = (ContentText txt) } = [ SimpleText txt ]

treeToPostParts _ = [ Skip ]

-- Forest == [ Tree Token ]
--
-- data Tree a = Node {
--     rootLabel :: a,         -- ^ label value
--     subForest :: [Tree a]   -- ^ zero or more child trees
-- }
--
-- Tree a == Tree Token
--
-- data Tree Token = Node {
--     rootLabel :: Token,         -- ^ label value
--     subForest :: [Tree Token]   -- ^ zero or more child trees
-- }
--
-- data Token
--  -- | An opening tag. Attribute ordering is arbitrary. Void elements have a 'TagOpen' but no corresponding 'TagClose'. See 'Text.HTML.Tree.nonClosing'.
--  = TagOpen !TagName [Attr]
--  -- | A self-closing tag.
--  | TagSelfClose !TagName [Attr]
--  -- | A closing tag.
--  | TagClose !TagName
--  -- | The content between tags.
--  | ContentText !Text
--  -- | A single character of content
--  | ContentChar !Char
--  -- | Contents of a comment.
--  | Comment !Builder
--  -- | Doctype
--  | Doctype !Text
--  deriving (Show, Ord, Eq, Generic)
