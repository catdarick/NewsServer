{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Database.Comment where
import qualified Api.Methods.Errors               as Err
import           Data.Int                         (Int64)
import           Data.Text                        (Text)
import           Database.PostgreSQL.Simple       (Connection, execute, query)
import           Database.PostgreSQL.Simple       (Only (Only))
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Database.PostgreSQL.Simple.Types (Only)
import           Database.Types
import Control.Exception (SomeException, try)
import Data.ByteString (ByteString)

addComment :: Connection -> NewsId -> UserId -> Content -> IO (Either ByteString [Only NewsId])
addComment conn newsId userId content = do
  [Only (commentId :: Int)] <-
    query
      conn
      [sql|
      INSERT INTO comment
      (user_id, content)
      VALUES (?,?) RETURNING id|]
      (userId, content)
  res <- try $ addAssociation conn newsId commentId
  case res of
      Left (e::SomeException) -> do 
          deleteComment conn commentId
          return $ Left Err.noNews
      Right _ -> return $ Right [Only commentId]


addAssociation :: Connection -> NewsId -> CommentId -> IO Int64
addAssociation conn newsId commentId = do
  execute
    conn
    [sql|
    INSERT INTO news_comment
    (news_id, comment_id)
    VALUES (?,?)|]
    (newsId, commentId)

deleteComment :: Connection -> CommentId -> IO Int64
deleteComment conn commentId = do
  execute
    conn
    [sql|
  DELETE FROM comment
  WHERE id=?|]
    (Only commentId)
