{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.Get.News where

import           Api.Types.Author
import           Api.Types.News
import           Data.Int                         (Int64)
import           Data.Text                        (Text)
import           Database.PostgreSQL.Simple       (query_, Connection, execute, query)
import           Database.PostgreSQL.Simple       (Only (Only))
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Database.PostgreSQL.Simple.Types (Binary (Binary))
import           Database.PostgreSQL.Simple.Types (Only)
import           Database.Types

getNews ::
     Connection
  -> Maybe AuthorId
  -> Maybe Login
  -> Maybe FirstName
  -> Maybe LastName
  -> Maybe CategoryId
  -> Maybe TagId
  -> Maybe [TagId]
  -> Maybe [TagId]
  -> Maybe Title
  -> Maybe Content
  -> Maybe Limit
  -> Maybe Offset
  -> IO [News]
getNews conn mbAuthorId mbLogin mbFName mbLName mbCategotyId mbTagId mbTagsIdIn mbTagsIdAll mbTitle mbContent mbLimit mbOffset = do
  res <-
    query_
      conn
      [sql|
                SELECT news.id, news.title, news.creation_time, news.content, news.main_picture, news.pictures,
                author.id, author.description, 
                usr.id, usr.login, usr.first_name, usr.last_name, usr.picture, usr.creation_time, usr.is_admin
                FROM news, user_account as usr, author
                WHERE news.author_id = author.id
                AND author.user_id = usr.id
                |]
      --(mbAuthorId, mbLogin, mbFName, mbLName, mbLimit, mbOffset)
  return $ map tupleToNews res
