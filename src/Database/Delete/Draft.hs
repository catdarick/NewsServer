{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Delete.Draft where

import           Data.Int                         (Int64)
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   execute)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Api.Types

deleteDraft :: Connection -> NewsId -> IO Int64
deleteDraft conn newsId =
  execute
    conn
    [sql|
  DELETE FROM news
  WHERE id=?|]
    (Only newsId)

deleteDraftTags :: Connection -> NewsId -> IO Int64
deleteDraftTags conn draftId =
  execute
    conn
    [sql|
          DELETE FROM news_tag
          WHERE news_id=?|]
    (Only draftId)
