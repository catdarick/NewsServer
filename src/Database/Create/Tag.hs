{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.Create.Tag where

import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Api.Types

addTag :: Connection -> Name -> IO [Only TagId]
addTag conn name =
  query
    conn
    [sql|
              INSERT INTO tag (name)
              VALUES (?) RETURNING id
              |]
    (Only name)
