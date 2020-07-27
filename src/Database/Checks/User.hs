{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.Checks.User where

import           Api.ErrorException
import           Api.Types.Synonyms
import           Control.Monad.Catch              (MonadThrow (throwM))
import           Control.Monad.Trans.Class        (MonadTrans (lift))
import           Control.Monad.Trans.State        (gets)
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   execute, query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Network.HTTP.Types               (status404)
import           State.Types

isAdminToken :: Token -> ServerStateIO Bool
isAdminToken token = do
  conn <- gets conn
  res <-
    lift $
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

adminGuard :: Token -> ServerStateIO ()
adminGuard token = do
  isAdmin <- isAdminToken token
  if isAdmin
    then return ()
    else throwM $ ErrorException status404 ""
