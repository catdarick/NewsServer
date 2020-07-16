{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.Tag where

import           Data.Int                         (Int64)
import           Data.Text                        (Text)
import           Database.PostgreSQL.Simple       (Connection, execute, query)
import           Database.PostgreSQL.Simple       (Only (Only))
import           Database.PostgreSQL.Simple       (In (In))
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Database.PostgreSQL.Simple.Types (Binary (Binary))
import           Database.PostgreSQL.Simple.Types (Only)
import           Database.Types

addTag :: Connection -> Name ->  IO [Only TagId]
addTag conn name =
      query
        conn
        [sql|
              INSERT INTO tag (name)
              VALUES (?) RETURNING id
              |]
        (Only name)


editTag :: Connection -> TagId -> Maybe Name -> IO Int64
editTag conn tagId mbName =
  execute
    conn
    [sql|
      UPDATE tag
      SET name = COALESCE(?, name)
      WHERE id=?|]
    (mbName, tagId)

deleteTag :: Connection -> TagId -> IO Int64
deleteTag conn tagId =
  execute
    conn
    [sql|
          DELETE FROM tag
          WHERE id=?|]
    (Only tagId)
