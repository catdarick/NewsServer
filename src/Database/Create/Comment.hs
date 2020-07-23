{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Database.Create.Comment where

import           Api.ErrorException
import qualified Api.Methods.Errors               as Err
import           Api.Types
import           Control.Exception                (try)
import           Control.Monad.Catch              (MonadThrow (throwM))
import           Database.Checks.Draft
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           GHC.Exception                    (SomeException,
                                                   errorCallException, throw)
import           Network.HTTP.Types.Status        (status400)

addComment :: Connection -> NewsId -> UserId -> Content -> IO CommentId
addComment conn newsId userId content = do
  isDraftPublished <- isDraftPublished conn newsId
  if isDraftPublished
    then do
      res <- query conn sqlReq (userId, content, newsId)
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
