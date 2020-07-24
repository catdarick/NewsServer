{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Database.Checks.Comment where

import           Api.Types.Synonyms
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)

isCommentCreator :: Connection -> CommentId -> Token -> IO Bool
isCommentCreator conn commentId token = do
  res <-
    query
      conn
      [sql|
        SELECT comment.id FROM user_token, comment
        WHERE comment.id = ?
        AND comment.user_id = user_token.user_id
        AND user_token.token = ?
        |]
      (commentId, token)
  case res of
    []               -> return False
    [id :: Only Int] -> return True
