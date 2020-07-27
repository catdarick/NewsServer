{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Database.Create.Author where

import           Api.ErrorException
import qualified Api.Errors                       as Err
import           Api.Types.Synonyms
import           Control.Exception                (SomeException, try)
import           Control.Monad.Catch              (MonadThrow (throwM))
import           Control.Monad.Trans.Class        (MonadTrans (lift))
import           Control.Monad.Trans.State        (gets)
import           Database.PostgreSQL.Simple       (Connection, execute, query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Database.PostgreSQL.Simple.Types (Only (Only))
import           Network.HTTP.Types               (status400)
import           State.Types

addAuthor :: UserId -> Maybe Description -> ServerStateIO AuthorId
addAuthor userId description = do
  conn <- gets conn
  res <-
    lift $
    try $
    query
      conn
      [sql|
          INSERT INTO author
          (user_id, description)
          VALUES (?,?) RETURNING id|]
      (userId, description)
  case res of
    Left (e :: SomeException) ->
      throwM $ ErrorException status400 Err.alreadyAuthor
    Right [Only id] -> return id
