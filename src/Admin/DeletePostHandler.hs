module Admin.DeletePostHandler
    ( deletePostHandler
    )
where

import Servant.Server (Handler)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (ExceptT (..), runExceptT)
import qualified Data.List.NonEmpty as L

import qualified Common.Network.ClientTypes as Client
import qualified DataClient as Client
import Common.Server.JSONSettings (JSONSettings)
import qualified Common.Network.SiteType as Site
import qualified Common.Network.BoardType as B
import qualified Common.Network.ThreadType as T
import qualified Common.Network.PostType as P
import qualified Common.AttachmentType as A


deletePostHandler :: JSONSettings -> Client.DeleteIllegalPostArgs -> Handler Client.DeleteIllegalPostArgs
deletePostHandler settings args@(Client.DeleteIllegalPostArgs post_id) = do
    liftIO $ putStrLn ("Hello DeleteIllegalPostArgs " <> show args)

    _ <- liftIO $ runExceptT $ do
        liftIO $ putStrLn "Hello ExceptT"
        sites <- ExceptT $ Client.getPost settings post_id

        let post_attachments = concatMap attachmentsFromSite sites

        liftIO $ print post_attachments

        -- TODO: - get attachments via site -> board ... -> attachments and use that !inner shit
        --     - also we can get the attachments on the /attachments, and use that inner shit to climb out till we hit site
        --     - then we can also get a post, or a thread, and have its sub contents and also it's parent contents loaded
        --     - all of these need to be parse-able from json, because they're all the same representation
        --     - can we use some of those type definitions that don't have lists of children to parse the intermediates, until we build Site at the end?

    pure args


attachmentsFromSite :: Site.Site -> [ A.Attachment ]
attachmentsFromSite
    = concatMap P.attachments
    . concatMap (L.toList . T.posts)
    . concatMap (L.toList . B.threads)
    . Site.boards
