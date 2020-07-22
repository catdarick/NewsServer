{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Database.Create.User where

import           Api.ErrorException
import qualified Api.Methods.Errors               as Err
import           Api.Types
import           Control.Exception                (SomeException)
import           Control.Exception                (try)
import           Control.Monad.Catch              (MonadThrow (throwM))
import           Data.Int                         (Int64)
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   execute, query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Database.PostgreSQL.Simple.Types (Binary (Binary))
import           Network.HTTP.Types.Status        (status400)

addUser ::
     Connection
  -> Login
  -> PassHash
  -> FirstName
  -> LastName
  -> Maybe Picture
  -> Bool
  -> IO UserId
addUser conn login passHash fName lName pictureId isAdmin = do
  res <-
    try $
    query
      conn
      [sql|
       INSERT INTO user_account
       (login, password_hash, first_name, last_name, picture, is_admin)
       VALUES (?,?,?,?,?,?) RETURNING id|]
      (login, Binary passHash, fName, lName, pictureId, isAdmin)
  case res of
    Left (e :: SomeException) -> throwM $ ErrorException status400 Err.loginBusy
    Right [Only id] -> return id

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
