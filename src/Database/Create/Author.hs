{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Create.Author where

import           Database.PostgreSQL.Simple       (Connection, execute, query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Database.PostgreSQL.Simple.Types (Only (Only))
import           Api.Types
import Control.Exception (SomeException)
import           Api.ErrorException
import qualified Api.Methods.Errors               as Err
import Control.Monad.Catch (MonadThrow(throwM))
import Network.HTTP.Types (status400)
import Control.Exception (try)
addAuthor :: Connection -> UserId -> Maybe Description -> IO AuthorId
addAuthor conn userId description = do
  res <- try $
    query
      conn
      [sql|
          INSERT INTO author
          (user_id, description)
          VALUES (?,?) RETURNING id|]
      (userId, description)
  case res of
    Left (e :: SomeException) -> throwM $ ErrorException status400 Err.alreadyAuthor
    Right [Only id] -> return id
