{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.Get.Comment where

import           Api.Types.Comment
import           Api.Types.Synonyms
import           Control.Monad.Trans.Class        (MonadTrans (lift))
import           Control.Monad.Trans.State        (gets)
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           State.Types

getComments :: NewsId -> Maybe Limit -> Maybe Offset -> ServerStateIO [Comment]
getComments newsId mbLimit mbOffset = do
  conn <- gets conn
  res <-
    lift $
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

getCommentCreator :: CommentId -> ServerStateIO [Only UserId]
getCommentCreator commentId = do
  conn <- gets conn
  lift $
    query
      conn
      [sql|
        SELECT user_id FROM comment
        WHERE id = ?|]
      (Only commentId)
