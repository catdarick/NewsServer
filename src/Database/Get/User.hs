{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.Get.User where

import           Api.Types
import           Api.Types.User
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   execute, query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Database.PostgreSQL.Simple.Types (Binary (Binary))

getUsers ::
     Connection
  -> Maybe UserId
  -> Maybe Login
  -> Maybe FirstName
  -> Maybe LastName
  -> Maybe Limit
  -> Maybe Offset
  -> IO [User]
getUsers conn mbUserId mbLogin mbFName mbLName mbLimit mbOffset = do
  res <-
    query
      conn
      [sql|
                SELECT id, login, first_name, last_name, picture, date_trunc('second',creation_time), is_admin
                FROM user_account
                WHERE id = COALESCE(?, id)
                AND login = COALESCE(?, login)
                AND first_name = COALESCE(?, first_name)
                AND last_name = COALESCE(?, last_name)
                LIMIT COALESCE(?, 50)
                OFFSET COALESCE(?, 0)
                |]
      (mbUserId, mbLogin, mbFName, mbLName, mbLimit, mbOffset)
  return $ map tupleToUser res

getMaybeUserIdAndPriv :: Connection -> Token -> IO [(UserId, IsAdmin)]
getMaybeUserIdAndPriv conn token =
  query
    conn
    [sql|
        SELECT user_token.user_id, user_account.is_admin FROM user_token, user_account
        WHERE user_token.token = ? AND user_account.id = user_token.user_id
        |]
    (Only token)

getMaybeUserId :: Connection -> Login -> PassHash -> IO [Only Int]
getMaybeUserId conn login passHash =
  query
    conn
    [sql|
        SELECT id FROM user_account
        WHERE login = ? AND password_hash = ?
        |]
    (login, Binary passHash)
