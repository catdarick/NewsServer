{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.Edit.Category where

import           Data.Int                         (Int64)
import           Database.PostgreSQL.Simple       (Connection, execute)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Api.Types

editCategory ::
     Connection -> CategoryId -> Maybe Name -> Maybe CategoryId -> IO Int64
editCategory conn categoryId mbName mbParentId =
  execute
    conn
    [sql|
      UPDATE category
      SET
      name = COALESCE(?, name),
      parent_id = COALESCE(?, parent_id)
      WHERE id=?|]
    (mbName, mbParentId, categoryId)
