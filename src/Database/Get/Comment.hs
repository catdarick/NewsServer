{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.Get.Comment where

import           Api.Types.Comment
import           Data.Int                         (Int64)
import           Data.Text                        (Text)
import           Data.Vector                      (fromList)
import           Database.PostgreSQL.Simple       (Connection, execute, query)
import           Database.PostgreSQL.Simple       (Only (Only))
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Database.PostgreSQL.Simple.Types (Binary (Binary))
import           Database.PostgreSQL.Simple.Types (Only)
import           Database.Types

getComments ::
     Connection -> NewsId -> Maybe Limit -> Maybe Offset -> IO [Comment]
getComments conn newsId mbLimit mbOffset = do
  res <-
    query
      conn
      [sql|
                SELECT comment.id, comment.content, date_trunc('second',comment.creation_time), comment.user_id,
                usr.login, usr.first_name, usr.last_name, usr.picture, date_trunc('second',usr.creation_time), usr.is_admin
                FROM comment, user_account as usr
                WHERE comment.news_id = ?
                AND usr.id=comment.user_id
                LIMIT COALESCE(?, 50)
                OFFSET COALESCE(?, 0)
                |]
      (newsId, mbLimit, mbOffset)
  return $ map tupleToComment res
