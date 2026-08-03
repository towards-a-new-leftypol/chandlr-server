{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use forM_" #-}

module Admin.DeletePostHandler
    ( deletePostHandler
    )
where

import Miso.String (fromMisoString)
import Servant.Server (Handler, err500, ServerError (..))
import Data.ByteString.Lazy.UTF8 (fromString)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import qualified Data.List.NonEmpty as L
import System.FilePath ((</>), (<.>))
import qualified System.Directory as Dir
import Control.Exception.Safe (catchAny)
import System.IO (hPutStrLn, stderr)
import Data.Maybe (fromMaybe)
import Control.Monad (when, unless)
import System.Directory (doesDirectoryExist, removeDirectoryRecursive)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import qualified Common.Server.JSONSettings as S
import qualified Common.FrontEnd.JSONSettings as JS
import Common.FrontEnd.Types (DeletePostResults (..))
import qualified Common.Network.ClientTypes as Client
import qualified Network.DataClient as Client
import qualified Common.Network.SiteType as Site
import qualified Common.Network.BoardType as B
import qualified Common.Network.ThreadType as T
import qualified Common.Network.PostType as P
import qualified Common.AttachmentType as A
import Common.Cookies (CookieJar (..))
import Network.SpamNoticer (addToKnownSpam)


deletePostHandler
    :: JS.JSONSettings
    -> Client.DeleteIllegalPostArgs
    -> Maybe CookieJar
    -> Handler DeletePostResults
deletePostHandler jssettings args@(Client.DeleteIllegalPostArgs post_id) mCookieJar = do
    liftIO $ putStrLn ("Hello DeleteIllegalPostArgs " <> show args)

    result <- liftIO $ runExceptT $ do
        liftIO $ putStrLn "Hello ExceptT"
        posts <- ExceptT $ Client.getPostById settings post_id

        let post_attachments = concatMap attachmentsFromSite posts

        liftIO $ print post_attachments

        let attachmentHashes = map A.sha256_hash post_attachments
        attachments <- ExceptT $ Client.getAttachmentsByHash settings attachmentHashes

        -- avoid duplicate posts: if the reported post has attachments, it's
        -- going to be in attachments
        let postsToDelete = if null post_attachments then posts
            else attachments

        let threadInfo = concatMap (findThreadInfo settings) postsToDelete

        noticerCounts <- liftIO $
            addToSpamNoticer jssettings adminName postsToDelete

        unless (null threadInfo) $ do
            liftIO $ do
                putStrLn $ "Deleting " ++ show (length threadInfo) ++ " threads."
                mapM_ (rmThreadDir . fst) threadInfo

            _ <- ExceptT $ Client.deleteThreads settings $ map snd threadInfo
            return ()

        liftIO $ do
            putStrLn "Deleting attachments and thumbnails:"
            mapM_ deleteAttachmentPair (concatMap (attachmentPathPairs settings) attachments)

        let postIds = concatMap collectPostIds postsToDelete
        _ <- ExceptT $ Client.deletePosts settings postIds

        return $ DeletePostResults postsToDelete noticerCounts

    case result of
        Left e -> throwError $ err500 { errBody = fromString $ show e }
        Right deletedPostInfo -> return deletedPostInfo

    where
        adminName :: Maybe Text.Text
        adminName = mCookieJar >>= \mCookie ->
            case mCookie of
                (CookieJar cookies) -> Map.lookup "mod" cookies >>= firstPart

        firstPart :: Text.Text -> Maybe Text.Text
        firstPart s
            | Text.null s = Nothing
            | otherwise =
                let split = Text.splitOn ":" s
                in case split of
                    []    -> Nothing
                    x:_ -> Just x

        settings = JS.clientSettings jssettings


-- Returns number of success and failure results obtained from submitting
-- posts to SpamNoticer
addToSpamNoticer :: JS.JSONSettings -> Maybe Text.Text -> [ Site.Site ] -> IO (Int, Int)
addToSpamNoticer jssettings adminName sites = do
    mapM f sites >>= return . countResults

    where
        f site =
            addToKnownSpam
                jssettings
                adminName
                site
                (map fst $ attachmentPathPairs settings site)

        settings = JS.clientSettings jssettings

        countResults :: [ Either a b ] -> (Int, Int)
        countResults = foldl' step (0, 0)
            where
                step (a, b) (Left  _) = (a, b + 1)
                step (a, b) (Right _) = (a + 1, b)


attachmentsFromSite :: Site.Site -> [ A.Attachment ]
attachmentsFromSite
    = concatMap P.attachments
    . concatMap (L.toList . T.posts)
    . concatMap B.threads
    . Site.boards


attachmentPathPairs :: S.JSONSettings -> Site.Site -> [(FilePath, Maybe FilePath)]
attachmentPathPairs settings site =
    [ ( threadDir </> (fileName <.> fileExt)
      , case A.thumb_extension attachment of
            Nothing -> Nothing
            Just ext -> Just $
                threadDir </> ("thumbnail_" ++ fileName <.> fromMisoString ext)
      )
    | board      <- L.toList (Site.boards site)
    , thread     <- B.threads board
    , post       <- L.toList (T.posts thread)
    , attachment <- P.attachments post
    , let fileName = fromMisoString (A.board_filename attachment)
          fileExt  = fromMisoString (fromMaybe "bin" $ A.file_extension attachment)
          threadDir = mediaRoot
            </> fromMisoString (Site.name site)
            </> fromMisoString (B.pathpart board)
            </> show (T.board_thread_id thread)
    ]

    where
        mediaRoot = S.media_root_path settings


deleteAttachmentPair :: (FilePath, Maybe FilePath) -> IO ()
deleteAttachmentPair (mainPath, maybeThumbPath) = do
    deleteIfExists "main attachment" mainPath

    case maybeThumbPath of
        Nothing -> return ()
        Just tp -> deleteIfExists "thumbnail" tp

  where
    deleteIfExists :: String -> FilePath -> IO ()
    deleteIfExists label path = do
        exists <- Dir.doesFileExist path

        when exists $
              catchAny (delete path) $ \e -> do
                  hPutStrLn stderr $
                      "Warning: Failed to delete " ++ label ++ " at " ++ path ++
                      ": " ++ show e

    delete :: FilePath -> IO ()
    delete p = do
        Dir.removeFile p
        putStrLn $ "Deleted " <> p


findThreadInfo :: S.JSONSettings -> Site.Site -> [(FilePath, Integer)]
findThreadInfo settings site =
    [ ( threadDir
      , T.thread_id thread
      )
    | board  <- L.toList (Site.boards site)
    , thread <- B.threads board
    , post   <- L.toList (T.posts thread)
    , P.board_post_id post == T.board_thread_id thread  -- OP condition
    , let threadDir =
            S.media_root_path settings
                </> fromMisoString (Site.name site)
                </> fromMisoString (B.pathpart board)
                </> show (T.board_thread_id thread)
    ]


collectPostIds :: Site.Site -> [ Integer ]
collectPostIds site =
    [ P.post_id post
    | board  <- L.toList (Site.boards site)
    , thread <- B.threads board
    , post   <- L.toList (T.posts thread)
    ]


rmThreadDir :: FilePath -> IO ()
rmThreadDir p = do
    exists <- doesDirectoryExist p
    when exists $ do
        removeDirectoryRecursive p
        putStrLn $ "Removed " <> p
