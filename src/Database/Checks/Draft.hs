{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Database.Checks.Draft where

import           Api.ErrorException
import qualified Api.Methods.Errors               as Err
import           Api.Types
import           Control.Monad.Catch              (MonadThrow (throwM))
import           Database.Get.Draft
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Network.HTTP.Types               (status403)

isDraftExists :: Connection -> NewsId -> IO Bool
isDraftExists conn newsId = do
  res <-
    query
      conn
      [sql|
        SELECT id FROM news
        WHERE id = ?
        |]
      (Only newsId)
  case res of
    []               -> return False
    [id :: Only Int] -> return True
  return True

isDraftAuthorToken :: Connection -> NewsId -> Token -> IO Bool
isDraftAuthorToken conn newsId token = do
  res <-
    query
      conn
      [sql|
        SELECT user_token.user_id FROM user_token, author, news
        WHERE news.id = ?
        AND news.author_id = author.id
        AND author.user_id = user_token.user_id
        AND user_token.token = ?
        |]
      (newsId, token)
  case res of
    []               -> return False
    [id :: Only Int] -> return True

isDraftPublished :: Connection -> NewsId -> IO Bool
isDraftPublished conn newsId = do
  res <-
    query
      conn
      [sql|
        SELECT id FROM news
        WHERE id = ?
        AND is_published=true
        |]
      (Only newsId)
  case res of
    []               -> return False
    [id :: Only Int] -> return True

draftAuthorGuard :: Connection -> NewsId -> Token -> IO ()
draftAuthorGuard conn draftId token = do
  isDraftAuthor <- isDraftAuthorToken conn draftId token
  if isDraftAuthor
    then return ()
    else throwM $ ErrorException status403 Err.noPerms
