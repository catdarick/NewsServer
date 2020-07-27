{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Database.Create.Comment where

import           Api.ErrorException
import qualified Api.Errors                       as Err
import           Api.Types.Synonyms
import           Control.Monad.Catch              (MonadThrow (throwM))
import           Control.Monad.Trans.Class        (MonadTrans (lift))
import           Control.Monad.Trans.State        (gets)
import           Database.Checks.Draft
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Network.HTTP.Types.Status        (status400)
import           State.Types

addComment :: NewsId -> UserId -> Content -> ServerStateIO CommentId
addComment newsId userId content = do
  isDraftPublished <- isDraftPublished newsId
  conn <- gets conn
  if isDraftPublished
    then do
      res <- lift $ query conn sqlReq (userId, content, newsId)
      return $ fromOnly res
    else throwM $ ErrorException status400 Err.noNews
  where
    fromOnly [Only id] = id
    sqlReq =
      [sql|
       INSERT INTO comment
       (user_id, content,news_id)
       VALUES (?,?,?) RETURNING id
      |]
