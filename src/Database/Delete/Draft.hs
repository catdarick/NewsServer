{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Delete.Draft where

import           Api.ErrorException
import qualified Api.Errors                       as Err
import           Api.Types.Synonyms
import           Control.Monad.Catch              (MonadThrow (throwM))
import           Control.Monad.Trans.Class        (MonadTrans (lift))
import           Control.Monad.Trans.State        (gets)
import           Data.Int                         (Int64)
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   execute)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Network.HTTP.Types.Status        (status400)
import           State.Types

deleteDraft :: NewsId -> ServerStateIO ()
deleteDraft newsId = do
  conn <- gets conn
  res <-
    lift $
    execute
      conn
      [sql|
    DELETE FROM news
    WHERE id=?
    AND is_published = false|]
      (Only newsId)
  case res of
    0 -> throwM $ ErrorException status400 Err.noDraft
    1 -> return ()

deleteDraftTags :: NewsId -> ServerStateIO Int64
deleteDraftTags draftId = do
  conn <- gets conn
  lift $
    execute
      conn
      [sql|
            DELETE FROM news_tag
            WHERE news_id=?|]
      (Only draftId)
