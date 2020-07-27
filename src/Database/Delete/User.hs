{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Database.Delete.User where

import           Api.ErrorException
import qualified Api.Errors                       as Err
import           Api.Types.Synonyms
import           Control.Monad.Catch              (MonadThrow (throwM))
import           Control.Monad.Trans.Class        (MonadTrans (lift))
import           Control.Monad.Trans.State        (gets)
import           Data.Int                         (Int64)
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   execute, query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Network.HTTP.Types.Status        (status400)
import           State.Types

deleteUser :: UserId -> ServerStateIO ()
deleteUser userId = do
  conn <- gets conn
  res <-
    lift $
    execute
      conn
      [sql|
              DELETE FROM user_account
              WHERE id=?|]
      (Only userId)
  case res of
    0 -> throwM $ ErrorException status400 Err.noUser
    1 -> return ()
