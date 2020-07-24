{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Delete.Tag where

import           Api.ErrorException
import qualified Api.Errors                       as Err
import           Api.Types.Synonyms
import           Control.Monad.Catch              (MonadThrow (throwM))
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   execute)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Network.HTTP.Types               (status400)

deleteTag :: Connection -> TagId -> IO ()
deleteTag conn tagId = do
  res <-
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
