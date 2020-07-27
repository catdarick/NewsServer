{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Database.Create.Tag where

import           Api.ErrorException
import qualified Api.Errors                       as Err
import           Api.Types.Synonyms
import           Control.Exception                (SomeException, try)
import           Control.Monad.Catch              (MonadThrow (throwM))
import           Control.Monad.Trans.Class        (MonadTrans (lift))
import           Control.Monad.Trans.State        (gets)
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Network.HTTP.Types               (status400)
import           State.Types

addTag :: Name -> ServerStateIO TagId
addTag name = do
  conn <- gets conn
  res <-
    lift $
    try $
    query
      conn
      [sql|
       INSERT INTO tag (name)
       VALUES (?) RETURNING id
       |]
      (Only name)
  case res of
    Left (e :: SomeException) -> throwM $ ErrorException status400 Err.tagExists
    Right [Only id] -> return id
