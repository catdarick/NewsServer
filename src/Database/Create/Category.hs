{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.Create.Category where

import           Api.ErrorException
import qualified Api.Errors                       as Err
import           Api.Types.Synonyms
import           Control.Monad.Catch              (MonadThrow (throwM))
import           Control.Monad.Trans.Class        (MonadTrans (lift))
import           Control.Monad.Trans.State        (gets)
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   execute, query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Network.HTTP.Types.Status        (status400)
import           State.Types

addCategory :: Name -> Maybe CategoryId -> ServerStateIO CategoryId
addCategory name parentId = do
  conn <- gets conn
  res <-
    lift $
    case parentId of
      Nothing ->
        query
          conn
          [sql|
          INSERT INTO category (name)
          VALUES (?) RETURNING id
          |]
          (Only name)
      Just _ ->
        query
          conn
          [sql|
          INSERT INTO category (name, parent_id)
          (SELECT ?, ?
          FROM category
          WHERE (id = ?)) RETURNING id
          |]
          (name, parentId, parentId)
  case res of
    []        -> throwM $ ErrorException status400 Err.noParent
    [Only id] -> return id
