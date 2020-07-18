{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Delete.Author where

import           Data.Int                         (Int64)
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   execute)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Api.Types

deleteAuthor :: Connection -> AuthorId -> IO Int64
deleteAuthor conn authorId =
  execute
    conn
    [sql|
          DELETE FROM author
          WHERE id=?|]
    (Only authorId)
