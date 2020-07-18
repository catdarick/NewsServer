{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Create.User where

import           Data.Int                         (Int64)
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   execute, query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Database.PostgreSQL.Simple.Types (Binary (Binary))
import           Api.Types

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
