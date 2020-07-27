{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Database.Edit.Tag where

import           Api.ErrorException
import qualified Api.Errors                       as Err
import           Api.Types.Synonyms
import           Control.Monad.Catch              (MonadThrow (throwM),
                                                   SomeException, try)
import           Control.Monad.Trans.Class        (MonadTrans (lift))
import           Control.Monad.Trans.State        (gets)
import           Database.PostgreSQL.Simple       (Connection, execute)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Network.HTTP.Types.Status        (status400)
import           State.Types

editTag :: TagId -> Maybe Name -> ServerStateIO ()
editTag tagId mbName = do
  conn <- gets conn
  res <-
    lift $
    try $
    execute
      conn
      [sql|
        UPDATE tag
        SET name = COALESCE(?, name)
        WHERE id=?|]
      (mbName, tagId)
  case res of
    Left (e :: SomeException) -> throwM $ ErrorException status400 Err.tagExists
    Right 0 -> throwM $ ErrorException status400 Err.noTag
    Right 1 -> return ()
