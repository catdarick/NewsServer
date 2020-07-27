{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Delete.Author where

import           Api.ErrorException
import qualified Api.Errors                       as Err
import           Api.Types.Synonyms
import           Control.Monad.Catch              (MonadThrow (throwM))
import           Control.Monad.Trans.Class        (MonadTrans (lift))
import           Control.Monad.Trans.State        (gets)
import           Data.Int                         (Int64)
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   execute)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Network.HTTP.Types.Status        (status400)
import           State.Types

deleteAuthor :: AuthorId -> ServerStateIO ()
deleteAuthor authorId = do
  conn <- gets conn
  res <-
    lift $
    execute
      conn
      [sql|
            DELETE FROM author
            WHERE id=?|]
      (Only authorId)
  case res of
    0 -> throwM $ ErrorException status400 Err.noAuthor
    1 -> return ()
