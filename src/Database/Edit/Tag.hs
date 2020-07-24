{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Edit.Tag where

import           Api.ErrorException
import qualified Api.Errors                       as Err
import           Api.Types.Synonyms
import           Control.Monad.Catch              (MonadThrow (throwM))
import           Database.PostgreSQL.Simple       (Connection, execute)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Network.HTTP.Types.Status        (status400)

editTag :: Connection -> TagId -> Maybe Name -> IO ()
editTag conn tagId mbName = do
  res <-
    execute
      conn
      [sql|
        UPDATE tag
        SET name = COALESCE(?, name)
        WHERE id=?|]
      (mbName, tagId)
  case res of
    0 -> throwM $ ErrorException status400 Err.noTag
    1 -> return ()
