{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Delete.Tag where

import           Api.ErrorException
import qualified Api.Errors                       as Err
import           Api.Types.Synonyms
import           Control.Monad.Catch              (MonadThrow (throwM))
import           Control.Monad.Trans.Class        (MonadTrans (lift))
import           Control.Monad.Trans.State        (gets)
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   execute)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Network.HTTP.Types               (status400)
import           State.Types

deleteTag :: TagId -> ServerStateIO ()
deleteTag tagId = do
  conn <- gets conn
  res <-
    lift $
    execute
      conn
      [sql|
        DELETE FROM tag
        WHERE id=?
      |]
      (Only tagId)
  case res of
    0 -> throwM $ ErrorException status400 Err.noTag
    1 -> return ()
