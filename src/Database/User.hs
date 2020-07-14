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
  -> Maybe Bool
  -> IO Int64
addUser conn login passHash fName lName pictureId isAdmin =
  execute
    conn
    [sql|
        INSERT INTO user_account
        (login, password_hash, first_name, second_name, picture, is_admin)
        VALUES (?,?,?,?,?,?)|]
    (login, Binary passHash, fName, lName, pictureId, isAdmin)

getMaybeUserId :: Connection -> Login -> PassHash -> IO [Only Int]
getMaybeUserId conn login passHash = do
  query
    conn
    [sql|
        SELECT id FROM user_account
        WHERE login = ? AND password_hash = ?
        |]
    (login, Binary passHash)

setToken :: Connection -> UserId -> Token -> IO Int64
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
