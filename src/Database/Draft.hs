{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Database.Draft where

import qualified Api.Methods.Errors               as Err
import           Control.Exception                (SomeException, try)
import           Control.Monad                    (void, when)
import           Data.ByteString                  (ByteString)
import           Data.Int                         (Int64)
import           Data.Maybe                       (isJust)
import           Data.Text                        (Text)
import           Data.Vector                      (fromList)
import           Database.PostgreSQL.Simple       (Connection, execute,
                                                   executeMany, query)
import           Database.PostgreSQL.Simple       (Only (Only))
import           Database.PostgreSQL.Simple       (In (In))
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Database.PostgreSQL.Simple.Types (Binary (Binary))
import           Database.PostgreSQL.Simple.Types (Only)
import           Database.Types

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
    Left (e :: SomeException) -> do
      print e
      return $ Left Err.noCategory
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
        (author_id, title, content, category_id, main_picture, pictures)
        VALUES (?,?,?,?,?,?) RETURNING id|]
    (authorId, title, content, caregoryId, mbPicture, fromList <$> mbPictures)

addDraftTags :: Connection -> NewsId -> Maybe [TagId] -> IO Int64
addDraftTags conn news_id mbTagsId = do
  case mbTagsId of
    Nothing -> return 1
    Just tagsId -> do
      let rows = map (\tag -> (news_id, tag)) tagsId
      executeMany
        conn
        [sql|
              INSERT INTO news_tag
              (news_id, tag_id)
              VALUES (?,?)|]
        rows

deleteDraft :: Connection -> NewsId -> IO Int64
deleteDraft conn newsId =
  execute
    conn
    [sql|
  DELETE FROM news
  WHERE id=?|]
    (Only newsId)

getDraftAuthorToken :: Connection -> NewsId -> IO [(Only Token)]
getDraftAuthorToken conn newsId =
  query
    conn
    [sql|
        SELECT user_token.token FROM user_token, author, news
        WHERE news.id = ?
        AND news.author_id = author.id
        AND author.user_id = user_token.user_id
        |]
    (Only newsId)

isDraftExists :: Connection -> NewsId -> IO Bool
isDraftExists conn newsId = do
  res <-
    query
      conn
      [sql|
        SELECT id FROM news
        WHERE id = ?
        |]
      (Only newsId)
  case res of
    []               -> return False
    [id :: Only Int] -> return True
  return True

isDraftAuthorToken :: Connection -> NewsId -> Token -> IO Bool
isDraftAuthorToken conn newsId token = do
  res <-
    query
      conn
      [sql|
        SELECT user_token.id FROM user_token, author, news
        WHERE news.id = ?
        AND news.author_id = author.id
        AND author.user_id = user_token.user_id
        AND user_token.token = ?
        |]
      (newsId, token)
  case res of
    []               -> return False
    [id :: Only Int] -> return True
  return True

publishDraft :: Connection -> NewsId -> IO Int64
publishDraft conn newsId =
  execute
    conn
    [sql|
      UPDATE news
      SET is_published = TRUE
      WHERE id=?|]
    (Only newsId)

deleteDraftTags :: Connection -> NewsId -> IO Int64
deleteDraftTags conn draftId =
  execute
    conn
    [sql|
          DELETE FROM news_tag
          WHERE news_id=?|]
    (Only draftId)

editDraft ::
     Connection
  -> NewsId
  -> Maybe Title
  -> Maybe Content
  -> Maybe CategoryId
  -> Maybe Picture
  -> Maybe [Picture]
  -> Maybe [TagId]
  -> IO ()
editDraft conn draftId mbTitle mbContent mbCategoryId mbPicture mbPictures mbTagsId = do
  execute
    conn
    [sql|
      UPDATE news
      SET
      title = COALESCE(?, title),
      content = COALESCE(?, content),
      category_id = COALESCE(?, category_id),
      main_picture = COALESCE(?, main_picture),
      pictures = COALESCE(?, pictures)
      WHERE id=?|]
    ( mbTitle
    , mbContent
    , mbCategoryId
    , mbPicture
    , fromList <$> mbPictures
    , draftId)
  when (isJust mbTagsId) $
    void $ do
      deleteDraftTags conn draftId
      addDraftTags conn draftId mbTagsId
