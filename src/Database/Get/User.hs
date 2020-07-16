{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.Get.User where

import           Data.Int                         (Int64)
import           Data.Text                        (Text)
import           Database.PostgreSQL.Simple       (Connection, execute, query)
import           Database.PostgreSQL.Simple       (Only (Only))
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Database.PostgreSQL.Simple.Types (Binary (Binary))
import           Database.PostgreSQL.Simple.Types (Only)
import           Database.Types
import           Api.Types.User

getUsers ::
     Connection
  -> Maybe UserId
  -> Maybe Login
  -> Maybe FirstName
  -> Maybe LastName
  -> Maybe Limit
  -> Maybe Offset
  -> IO [User]
getUsers conn mbUserId mbLogin mbFName mbLName mbLimit offset= do
  res <-
    query
      conn
      [sql|
                SELECT id, login, first_name, last_name, picture, creation_time, is_admin
                FROM user_account
                WHERE id = COALESCE(?, id)
                AND login = COALESCE(?, login)
                AND first_name = COALESCE(?, first_name)
                AND last_name = COALESCE(?, last_name)
                LIMIT COALESCE(?, 100) 
                OFFSET COALESCE(?, 0) 
                |]
      (mbUserId, mbLogin, mbFName, mbLName, mbLimit, offset)
  return $ map tupleToUser res
