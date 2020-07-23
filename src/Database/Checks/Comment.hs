{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Database.Checks.Comment where

import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Api.Types
import Control.Monad.Catch (MonadThrow(throwM))
import           Api.ErrorException
import qualified Api.Methods.Errors             as Err
import Network.HTTP.Types (status403)
import Database.Get.Draft

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
