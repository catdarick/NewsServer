{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Database.Create.User where

import           Api.ErrorException
import qualified Api.Errors                       as Err
import           Api.Types.Synonyms
import           Control.Exception                (SomeException, try)
import           Control.Monad.Catch              (MonadThrow (throwM))
import           Control.Monad.Trans.Class        (MonadTrans (lift))
import           Control.Monad.Trans.State        (gets)
import           Data.Int                         (Int64)
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   execute, query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Database.PostgreSQL.Simple.Types (Binary (Binary))
import           Network.HTTP.Types.Status        (status400)
import           State.Types

addUser ::
     Login
  -> PassHash
  -> FirstName
  -> LastName
  -> Maybe Picture
  -> Bool
  -> ServerStateIO UserId
addUser login passHash fName lName pictureId isAdmin = do
  conn <- gets conn
  res <-
    lift $
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

setToken :: UserId -> TokenString -> ServerStateIO Int64
setToken userId token = do
  conn <- gets conn
  lift $
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
