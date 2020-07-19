{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.Create.Category where

import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   execute, query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Api.Types

addCategory :: Connection -> Name -> Maybe CategoryId -> IO [Only CategoryId]
addCategory conn name parentId = 
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
