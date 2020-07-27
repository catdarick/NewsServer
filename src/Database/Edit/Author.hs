{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Edit.Author where

import           Api.ErrorException
import qualified Api.Errors                       as Err
import           Api.Types.Synonyms
import           Control.Monad.Catch              (MonadThrow (throwM))
import           Control.Monad.Trans.Class        (MonadTrans (lift))
import           Control.Monad.Trans.State        (gets)
import           Database.PostgreSQL.Simple       (Connection, execute)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Database.PostgreSQL.Simple.Types (Only (Only))
import           Network.HTTP.Types.Status        (status400)
import           State.Types

editAuthor :: AuthorId -> Maybe Description -> ServerStateIO ()
editAuthor auhorId mbDescription = do
  conn <- gets conn
  res <-
    lift $
    execute
      conn
      [sql|
            UPDATE author
            SET description = COALESCE(?, description)
            WHERE id=?|]
      (mbDescription, auhorId)
  case res of
    0 -> throwM $ ErrorException status400 Err.noAuthor
    1 -> return ()
