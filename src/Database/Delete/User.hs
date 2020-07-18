{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Delete.User where

import           Data.Int                         (Int64)
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   execute, query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Api.Types

deleteUser :: Connection -> UserId -> IO Int64
deleteUser conn userId =
  execute
    conn
    [sql|
          DELETE FROM user_account
          WHERE id=?|]
    (Only userId)
