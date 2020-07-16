{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.User where

import           Data.Int                         (Int64)
import           Data.Text                        (Text)
import           Database.PostgreSQL.Simple       (Connection, execute, query)
import           Database.PostgreSQL.Simple       (Only (Only))
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Database.PostgreSQL.Simple.Types (Binary (Binary))
import           Database.PostgreSQL.Simple.Types (Only)
import           Database.Types

addUser ::
     Connection
  -> Login
  -> PassHash
  -> FirstName
  -> LastName
  -> Maybe Picture
  -> Bool
  -> IO [Only UserId]
addUser conn login passHash fName lName pictureId isAdmin =
  query
    conn
    [sql|
        INSERT INTO user_account
        (login, password_hash, first_name, last_name, picture, is_admin)
        VALUES (?,?,?,?,?,?) RETURNING id|]
    (login, Binary passHash, fName, lName, pictureId, isAdmin)

deleteUser :: Connection -> UserId -> IO Int64
deleteUser conn userId =
  execute
    conn
    [sql|
          DELETE FROM user_account
          WHERE id=?|]
    (Only userId)

getMaybeUserId :: Connection -> Login -> PassHash -> IO [Only Int]
getMaybeUserId conn login passHash = do
  query
    conn
    [sql|
        SELECT id FROM user_account
        WHERE login = ? AND password_hash = ?
        |]
    (login, Binary passHash)

setToken :: Connection -> UserId -> TokenString -> IO Int64
setToken conn userId token =
  execute
    conn
    [sql|
        INSERT INTO user_token
        (user_id, token)
        VALUES (?,?)
        ON CONFLICT (user_id) DO UPDATE
        SET token = excluded.token
        |]
    (userId, token)

getMaybeUserIdAndPriv :: Connection -> Token -> IO [(UserId, IsAdmin)]
getMaybeUserIdAndPriv conn token =
  query
    conn
    [sql|
        SELECT user_token.user_id, user_account.is_admin FROM user_token, user_account
        WHERE user_token.token = ? AND user_account.id = user_token.user_id
        |]
    (Only token)

isAdminToken :: Connection -> Token -> IO Bool
isAdminToken conn token = do
  res <-
    query
      conn
      [sql|
        SELECT user_account.is_admin FROM user_token, user_account
        WHERE user_token.token = ? AND user_account.id = user_token.user_id
        |]
      (Only token)
  case res of
    [Only True] -> return True
    _           -> return False
