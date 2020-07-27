{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.Get.User where

import           Api.ErrorException
import qualified Api.Errors                       as Err
import           Api.Types.Synonyms
import           Api.Types.User
import           Control.Monad.Catch              (MonadThrow (throwM))
import           Control.Monad.Trans.Class        (MonadTrans (lift))
import           Control.Monad.Trans.State        (gets)
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   execute, query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Database.PostgreSQL.Simple.Types (Binary (Binary))
import           Network.HTTP.Types               (status400, status401)
import           State.Types

getUsers ::
     Maybe UserId
  -> Maybe Login
  -> Maybe FirstName
  -> Maybe LastName
  -> Maybe Limit
  -> Maybe Offset
  -> ServerStateIO [User]
getUsers mbUserId mbLogin mbFName mbLName mbLimit mbOffset = do
  conn <- gets conn
  res <-
    lift $
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

getMaybeUserIdAndPriv :: Token -> ServerStateIO [(UserId, IsAdmin)]
getMaybeUserIdAndPriv token = do
  conn <- gets conn
  lift $
    query
      conn
      [sql|
        SELECT user_token.user_id, user_account.is_admin FROM user_token, user_account
        WHERE user_token.token = ? AND user_account.id = user_token.user_id
        |]
      (Only token)

getUserId :: Token -> ServerStateIO UserId
getUserId token = do
  conn <- gets conn
  res <-
    lift $
    query
      conn
      [sql|
          SELECT user_id FROM user_token
          WHERE token = ?
          |]
      (Only token)
  case res of
    []        -> throwM $ ErrorException status401 Err.badToken
    [Only id] -> return id

getUserIdByPass :: Login -> PassHash -> ServerStateIO Int
getUserIdByPass login passHash = do
  conn <- gets conn
  res <-
    lift $
    query
      conn
      [sql|
          SELECT id FROM user_account
          WHERE login = ? AND password_hash = ?
          |]
      (login, Binary passHash)
  case res of
    []        -> throwM $ ErrorException status400 Err.badPassword
    [Only id] -> return id
