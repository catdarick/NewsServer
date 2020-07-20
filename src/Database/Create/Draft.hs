{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module Database.Create.Draft where

import qualified Api.Methods.Errors               as Err
import           Api.Types
import           Control.Exception                (SomeException, try)
import           Data.ByteString                  (ByteString)
import           Data.Int                         (Int64)
import           Data.Vector                      (fromList)
import           Database.Delete.Draft
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   execute, executeMany, query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)

addDraftWithTags ::
     Connection
  -> AuthorId
  -> Title
  -> Content
  -> CategoryId
  -> Maybe Picture
  -> Maybe [Picture]
  -> Maybe [TagId]
  -> IO (Either ByteString [Only NewsId])
addDraftWithTags conn authorId title content caregoryId mbPicture mbPictures mbTagsId = do
  resAddNews <-
    try $ addDraft conn authorId title content caregoryId mbPicture mbPictures
  case resAddNews of
    Left (e :: SomeException) -> return $ Left Err.noCategory
    Right [Only id] -> do
      resAddTags <- try $ addDraftTags conn id mbTagsId
      case resAddTags of
        Left (e :: SomeException) -> do
          deleteDraft conn id
          return $ Left Err.badTags
        Right _ -> return $ Right [Only id]

addDraft ::
     Connection
  -> AuthorId
  -> Title
  -> Content
  -> CategoryId
  -> Maybe Picture
  -> Maybe [Picture]
  -> IO [Only NewsId]
addDraft conn authorId title content caregoryId mbPicture mbPictures =
  query
    conn
    [sql|
        INSERT INTO news
        (author_id, title, content, category_id, main_picture, pictures, is_published)
        VALUES (?,?,?,?,?,?, false) RETURNING id|]
    (authorId, title, content, caregoryId, mbPicture, fromList <$> mbPictures)

addDraftTags :: Connection -> NewsId -> Maybe [TagId] -> IO Int64
addDraftTags conn news_id mbTagsId =
  case mbTagsId of
    Nothing -> return 1
    Just tagsId -> do
      let rows = map (news_id, ) tagsId
      executeMany
        conn
        [sql|
              INSERT INTO news_tag
              (news_id, tag_id)
              VALUES (?,?)|]
        rows

publishDraft :: Connection -> NewsId -> IO Int64
publishDraft conn newsId =
  execute
    conn
    [sql|
      UPDATE news
      SET is_published = TRUE, creation_time = CURRENT_TIMESTAMP
      WHERE id=?|]
    (Only newsId)
