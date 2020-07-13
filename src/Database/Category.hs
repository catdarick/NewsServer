{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.Category where

import           Database.PostgreSQL.Simple       (Only (Only), Query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)

sqlReqCategoryHierarchy =
  [sql| WITH RECURSIVE r AS (
            SELECT id, parent_id, title
            FROM category
            WHERE id = ?
            UNION
            SELECT category.id, category.parent_id, category.title
            FROM category
                JOIN r
                    ON category.id = r.parent_id
            )
            SELECT * FROM r|]
