{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Database.Delete.User where

import           Api.ErrorException
import qualified Api.Methods.Errors               as Err
import           Api.Types
import           Control.Exception                (SomeException, try)
import           Control.Monad.Catch              (MonadThrow (throwM))
import           Data.Int                         (Int64)
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   execute, query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Network.HTTP.Types.Status        (status400)

deleteUser :: Connection -> UserId -> IO ()
deleteUser conn userId = do
  res <-
    execute
      conn
      [sql|
              DELETE FROM user_account
              WHERE id=?|]
      (Only userId)
  case res of
    0 -> throwM $ ErrorException status400 Err.noUser
    1 -> return ()
