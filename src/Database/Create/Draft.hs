{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module Database.Create.Draft where

import           Api.ErrorException
import qualified Api.Errors                       as Err
import           Api.Types.Synonyms
import           Control.Exception                (SomeException, try)
import           Control.Monad.Catch              (MonadThrow (throwM))
import           Data.ByteString                  (ByteString)
import           Data.Int                         (Int64)
import           Data.Vector                      (fromList)
import           Database.Delete.Draft
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   execute, executeMany, query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Network.HTTP.Types.Status        (status400)

addDraftWithTags ::
     Connection
  -> AuthorId
  -> Title
  -> Content
  -> CategoryId
  -> Maybe Picture
  -> Maybe [Picture]
  -> Maybe [TagId]
  -> IO NewsId
addDraftWithTags conn authorId title content caregoryId mbPicture mbPictures mbTagsId = do
  id <- addDraft conn authorId title content caregoryId mbPicture mbPictures
  resAddTags <- try $ addDraftTags conn id mbTagsId
  case resAddTags of
    Left (e :: ErrorException) -> do
      deleteDraft conn id
      throwM e
    Right _ -> return id

addDraft ::
     Connection
  -> AuthorId
  -> Title
  -> Content
  -> CategoryId
  -> Maybe Picture
  -> Maybe [Picture]
  -> IO NewsId
addDraft conn authorId title content caregoryId mbPicture mbPictures = do
  res <-
    try $
    query
      conn
      [sql|
          INSERT INTO news
          (author_id, title, content, category_id, main_picture, pictures, is_published)
          VALUES (?,?,?,?,?,?, false) RETURNING id|]
      (authorId, title, content, caregoryId, mbPicture, fromList <$> mbPictures)
  case res of
    Left (e :: SomeException) ->
      throwM $ ErrorException status400 Err.noCategory
    Right [Only id] -> return id

addDraftTags :: Connection -> NewsId -> Maybe [TagId] -> IO ()
addDraftTags conn news_id mbTagsId =
  case mbTagsId of
    Nothing -> return ()
    Just tagsId -> do
      let rows = map (news_id, ) tagsId
      res <-
        try $
        executeMany
          conn
          [sql|
                  INSERT INTO news_tag
                  (news_id, tag_id)
                  VALUES (?,?)|]
          rows
      case res of
        Left (e :: SomeException) ->
          throwM $ ErrorException status400 Err.badTags
        Right _ -> return ()

publishDraft :: Connection -> NewsId -> IO ()
publishDraft conn newsId = do
  res <-
    execute
      conn
      [sql|
        UPDATE news
        SET is_published = TRUE, creation_time = CURRENT_TIMESTAMP
        WHERE id=?|]
      (Only newsId)
  case res of
    0 -> throwM $ ErrorException status400 Err.noDraft
    1 -> return ()