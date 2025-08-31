{-# LANGUAGE OverloadedStrings #-}

module Parsing.BodyParser where

import Data.Text (Text)
import qualified Data.Text.IO as Text
import Text.HTML.Parser (parseTokens, canonicalizeTokens, Token (..), Attr(..))
import Text.HTML.Tree (tokensToForest)
import Data.Tree (Forest, Tree (..))

getAttr :: Text -> [ Attr ] -> Maybe Text
getAttr _ [] = Nothing
getAttr attrName (Attr x y:xs)
    | x ==  attrName = Just y
    | otherwise = getAttr attrName xs

parsePostBody :: Text -> IO ()
parsePostBody html = 
    case tokensToForest $ canonicalizeTokens $ parseTokens html of
        Left err -> do
            print err

        Right forest -> forestToPostParts forest


forestToPostParts :: Forest Token -> IO ()
forestToPostParts = mapM_ treeToPostParts

treeToPostParts :: Tree Token -> IO ()
treeToPostParts Node { rootLabel = tok@(TagOpen "a" attrs) } =
    do
        putStrLn "Anchor Tag Open"
        print attrs
        print $ getAttr "href" attrs

treeToPostParts Node { rootLabel = tok@(TagSelfClose tagname attrs) } =
    do
        putStrLn "Tag Self-Close"
        putStrLn $ "tagname: " ++ show tagname
        putStrLn $ "attrs: " ++ show attrs

-- treeToPostParts Node { rootLabel = (ContentText txt) } = Text.putStrLn txt

treeToPostParts node@Node { rootLabel } =
    do
        putStrLn $ "Something Else: " ++ show rootLabel

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
-- and what is a Token? We might need to make a toy program with this
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

-- Add your main method here
main :: IO ()
main = do
    -- Read the contents of the file
    fileContents <- Text.readFile "/home/phil/Documents/haskell/chandlr-miso/html/body.html"

    parsePostBody fileContents
