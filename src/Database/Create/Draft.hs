{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module Database.Create.Draft where

import           Api.ErrorException
import qualified Api.Errors                       as Err
import           Api.Types.Synonyms
import           Control.Exception                (SomeException)
import           Control.Monad.Catch              (MonadThrow (throwM))
import           Control.Monad.Catch.Pure         (try)
import           Control.Monad.Trans.Class        (MonadTrans (lift))
import           Control.Monad.Trans.State        (gets)
import           Data.ByteString                  (ByteString)
import           Data.Int                         (Int64)
import           Data.Vector                      (fromList)
import           Database.Delete.Draft
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   execute, executeMany, query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Network.HTTP.Types.Status        (status400)
import           State.Types

addDraftWithTags ::
     AuthorId
  -> Title
  -> Content
  -> CategoryId
  -> Maybe Picture
  -> Maybe [Picture]
  -> Maybe [TagId]
  -> ServerStateIO NewsId
addDraftWithTags authorId title content caregoryId mbPicture mbPictures mbTagsId = do
  id <- addDraft authorId title content caregoryId mbPicture mbPictures
  resAddTags <- try $ addDraftTags id mbTagsId
  case resAddTags of
    Left (e :: ErrorException) -> do
      deleteDraft id
      throwM e
    Right _ -> return id

addDraft ::
     AuthorId
  -> Title
  -> Content
  -> CategoryId
  -> Maybe Picture
  -> Maybe [Picture]
  -> ServerStateIO NewsId
addDraft authorId title content caregoryId mbPicture mbPictures = do
  conn <- gets conn
  res <-
    lift $
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

addDraftTags :: NewsId -> Maybe [TagId] -> ServerStateIO ()
addDraftTags news_id mbTagsId = do
  conn <- gets conn
  case mbTagsId of
    Nothing -> return ()
    Just tagsId -> do
      let rows = map (news_id, ) tagsId
      res <-
        lift $
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

publishDraft :: NewsId -> ServerStateIO ()
publishDraft newsId = do
  conn <- gets conn
  res <-
    lift $
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
