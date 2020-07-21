{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Create.Comment where

import           Api.Types
import           Database.Checks.Draft
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           GHC.Exception                    (errorCallException, throw)

addComment :: Connection -> NewsId -> UserId -> Content -> IO [Only CommentId]
addComment conn newsId userId content = do
  isDraftPublished <- isDraftPublished conn newsId
  if isDraftPublished
    then query
           conn
           [sql|
            INSERT INTO comment
            (user_id, content,news_id)
            VALUES (?,?,?) RETURNING id|]
           (userId, content, newsId)
    else throw $ errorCallException "Not published"
