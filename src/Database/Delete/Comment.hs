{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Delete.Comment where

import           Data.Int                         (Int64)
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   execute)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Api.Types

deleteComment :: Connection -> CommentId -> IO Int64
deleteComment conn commentId =
  execute
    conn
    [sql|
  DELETE FROM comment
  WHERE id=?|]
    (Only commentId)
