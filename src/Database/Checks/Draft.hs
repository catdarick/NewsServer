{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Database.Checks.Draft where

import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Api.Types

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
        SELECT user_token.id FROM user_token, author, news
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
