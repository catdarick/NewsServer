{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Delete.Draft where

import           Api.ErrorException
import qualified Api.Methods.Errors               as Err
import           Api.Types
import           Control.Monad.Catch              (MonadThrow (throwM))
import           Data.Int                         (Int64)
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   execute)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Network.HTTP.Types.Status        (status400)

deleteDraft :: Connection -> NewsId -> IO ()
deleteDraft conn newsId = do
  res <-
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

deleteDraftTags :: Connection -> NewsId -> IO Int64
deleteDraftTags conn draftId =
  execute
    conn
    [sql|
          DELETE FROM news_tag
          WHERE news_id=?|]
    (Only draftId)
