{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Edit.Tag where

import           Data.Int                         (Int64)
import           Database.PostgreSQL.Simple       (Connection, execute)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Api.Types

editTag :: Connection -> TagId -> Maybe Name -> IO Int64
editTag conn tagId mbName =
  execute
    conn
    [sql|
      UPDATE tag
      SET name = COALESCE(?, name)
      WHERE id=?|]
    (mbName, tagId)
