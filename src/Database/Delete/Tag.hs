{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.Delete.Tag where

import           Data.Int                         (Int64)
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   execute, query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Api.Types

deleteTag :: Connection -> TagId -> IO Int64
deleteTag conn tagId =
  execute
    conn
    [sql|
          DELETE FROM tag
          WHERE id=?|]
    (Only tagId)
