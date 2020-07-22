{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Delete.Author where

import           Data.Int                         (Int64)
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   execute)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Api.Types
import           Api.ErrorException
import qualified Api.Methods.Errors             as Err
import Control.Monad.Catch (MonadThrow(throwM))
import Network.HTTP.Types.Status (status404)
deleteAuthor :: Connection -> AuthorId -> IO ()
deleteAuthor conn authorId = do
  res <-
    execute
      conn
      [sql|
            DELETE FROM author
            WHERE id=?|]
      (Only authorId)
  case res of
    0 -> throwM $ ErrorException status404 Err.noAuthor
    1 -> return ()
