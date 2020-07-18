{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Create.Author where

import           Database.PostgreSQL.Simple       (Connection, execute, query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Database.PostgreSQL.Simple.Types (Only (Only))
import           Api.Types

addAuthor :: Connection -> UserId -> Maybe Description -> IO [Only AuthorId]
addAuthor conn userId description =
  query
    conn
    [sql|
        INSERT INTO author
        (user_id, description)
        VALUES (?,?) RETURNING id|]
    (userId, description)
