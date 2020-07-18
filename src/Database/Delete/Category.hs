{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Delete.Category where

import           Data.Int                         (Int64)
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   execute)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Api.Types

deleteCategory :: Connection -> CategoryId -> IO Int64
deleteCategory conn categoryId =
  execute
    conn
    [sql|
          DELETE FROM category
          WHERE id=?|]
    (Only categoryId)
