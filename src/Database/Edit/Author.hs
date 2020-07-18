{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.Edit.Author where

import           Data.Int                         (Int64)
import           Database.PostgreSQL.Simple       (Connection, execute)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Api.Types

editAuthor :: Connection -> AuthorId -> Maybe Description -> IO Int64
editAuthor conn auhorId mbDescription =
  execute
    conn
    [sql|
      UPDATE author
      SET description = COALESCE(?, description)
      WHERE id=?|]
    (mbDescription, auhorId)
