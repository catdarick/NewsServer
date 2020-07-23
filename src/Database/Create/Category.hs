{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.Create.Category where

import           Api.ErrorException
import qualified Api.Methods.Errors               as Err
import           Api.Types
import           Control.Monad.Catch              (MonadThrow (throwM))
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   execute, query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Network.HTTP.Types.Status        (status400)

addCategory :: Connection -> Name -> Maybe CategoryId -> IO CategoryId
addCategory conn name parentId = do
  res <-
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
