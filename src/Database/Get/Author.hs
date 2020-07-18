{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.Get.Author where

import           Api.Types.Author
import           Data.Int                         (Int64)
import           Data.Text                        (Text)
import           Database.Get.User
import           Database.PostgreSQL.Simple       (Connection, execute, query)
import           Database.PostgreSQL.Simple       (Only (Only))
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Database.PostgreSQL.Simple.Types (Binary (Binary))
import           Database.PostgreSQL.Simple.Types (Only)
import           Database.Types

getAuthors ::
     Connection
  -> Maybe AuthorId
  -> Maybe UserId
  -> Maybe Login
  -> Maybe FirstName
  -> Maybe LastName
  -> Maybe Limit
  -> Maybe Offset
  -> IO [Author]
getAuthors conn mbAuthorId mbUserId mbLogin mbFName mbLName mbLimit mbOffset = do
  res <-
    query
      conn
      [sql|
                SELECT author.id, author.description,
                usr.id, usr.login, usr.first_name, usr.last_name, usr.picture, date_trunc('second',usr.creation_time), usr.is_admin
                FROM author, user_account as usr
                WHERE author.id = COALESCE(?, author.id)
                AND author.user_id = COALESCE(?, author.user_id)
                AND usr.login = COALESCE(?, usr.login)
                AND usr.first_name = COALESCE(?, usr.first_name)
                AND usr.last_name = COALESCE(?, usr.last_name)
                AND author.user_id = usr.id
                LIMIT COALESCE(?, 50)
                OFFSET COALESCE(?, 0)
                |]
      (mbAuthorId, mbUserId, mbLogin, mbFName, mbLName, mbLimit, mbOffset)
  return $ map tupleToAuthor res
