{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Database.Checks.Comment where

import           Api.Types.Synonyms
import           Control.Monad.Trans.Class        (MonadTrans (lift))
import           Control.Monad.Trans.State        (gets)
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           State.Types

isCommentCreator :: CommentId -> Token -> ServerStateIO Bool
isCommentCreator commentId token = do
  conn <- gets conn
  res <-
    lift $
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
