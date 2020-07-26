{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Database.Edit.Tag where

import           Api.ErrorException
import qualified Api.Errors                       as Err
import           Api.Types.Synonyms
import           Control.Monad.Catch              (MonadThrow (throwM),
                                                   SomeException, try)
import           Database.PostgreSQL.Simple       (Connection, execute)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Network.HTTP.Types.Status        (status400)

editTag :: Connection -> TagId -> Maybe Name -> IO ()
editTag conn tagId mbName = do
  res <-
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
