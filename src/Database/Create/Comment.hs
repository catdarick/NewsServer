{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Create.Comment where

import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Api.Types

addComment :: Connection -> NewsId -> UserId -> Content -> IO [Only CommentId]
addComment conn newsId userId content =
  query
    conn
    [sql|
      INSERT INTO comment
      (user_id, content,news_id)
      VALUES (?,?,?) RETURNING id|]
    (userId, content, newsId)
