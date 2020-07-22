{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.Checks.User where

import           Api.ErrorException
import           Api.Types
import           Control.Monad.Catch              (MonadThrow (throwM))
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   execute, query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Network.HTTP.Types               (status404)

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

adminGuard :: Connection -> Token -> IO ()
adminGuard conn token = do
  isAdmin <- isAdminToken conn token
  if isAdmin
    then return ()
    else throwM $ ErrorException status404 ""
