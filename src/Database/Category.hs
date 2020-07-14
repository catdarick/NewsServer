{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.Category where

import           Data.Int                         (Int64)
import           Data.Text                        (Text)
import           Database.PostgreSQL.Simple       (Connection, execute, query)
import           Database.PostgreSQL.Simple       (Only (Only))
import           Database.PostgreSQL.Simple       (In (In))
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Database.PostgreSQL.Simple.Types (Binary (Binary))
import           Database.PostgreSQL.Simple.Types (Only)
import           Database.Types

addCategory :: Connection -> Name -> Maybe CategoryId -> IO Int64
addCategory conn name parentId =
  case parentId of
    Nothing ->
      execute
        conn
        [sql|
              INSERT INTO category (name)
              VALUES (?)
              |]
        (Only name)
    Just _ ->
      execute
        conn
        [sql|
              INSERT INTO category (name, parent_id)
              SELECT ?, ?
              FROM category
              WHERE (id = ?);
              |]
        (name, parentId, parentId)

sqlReqCategoryHierarchy =
  [sql| WITH RECURSIVE r AS (
            SELECT id, parent_id, name
            FROM category
            WHERE id = 5
            UNION
            SELECT category.id, category.parent_id, category.name
            FROM category
                JOIN r
                    ON category.id = r.parent_id
            )
            SELECT * FROM r|]
