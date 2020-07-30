{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.Get.Draft where

import           Api.Types.Author
import           Api.Types.News
import           Api.Types.Synonyms
import           Control.Monad.Trans.Class        (MonadTrans (lift))
import           Control.Monad.Trans.State        (gets)
import           Data.Vector                      (fromList, toList)
import           Database.Get.Category
import           Database.Get.Tag
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           State.Types

getDrafts ::
     Token
  -> Maybe NewsId
  -> Maybe CategoryId
  -> Maybe TagId
  -> Maybe [TagId]
  -> Maybe [TagId]
  -> Maybe Title
  -> Maybe Content
  -> Maybe Limit
  -> Maybe Offset
  -> ServerStateIO [News]
getDrafts token mbDraftId mbCategotyId mbTagId mbTagsIdIn mbTagsIdAll mbTitle mbContent mbLimit mbOffset = do
  conn <- gets conn
  res <-
    lift $
    query
      conn
      [sql|
      SELECT * FROM
        (SELECT news.id, news.title, date_trunc('second',news.creation_time),
        news.content, news.main_picture, news.pictures, news.category_id,
        (SELECT ARRAY(select news_tag.tag_id from news_tag
          where news_tag.news_id = news.id)) as tagsId
        FROM news, author, user_token
        WHERE news.is_published = false
        AND news.id =  COALESCE(?, news.id)
        AND user_token.user_id =  author.user_id
        AND news.author_id = author.id
        AND news.title LIKE ?
        AND news.content LIKE ?
        AND user_token.token=?
        AND news.category_id = COALESCE(?, news.category_id)
        LIMIT COALESCE(?, 50)
        OFFSET COALESCE(?, 0)) as foo
      WHERE  (? IS NULL OR (? = ANY (foo.tagsId)))
      AND (? IS NULL OR (? <@ (foo.tagsId)))
      AND (? IS NULL OR (? && (foo.tagsId)))
      |]
      ( mbDraftId
      , toTemplate mbTitle
      , toTemplate mbContent
      , token
      , mbCategotyId
      , mbLimit
      , mbOffset
      , mbTagId
      , mbTagId
      , fromList <$> mbTagsIdAll
      , fromList <$> mbTagsIdAll
      , fromList <$> mbTagsIdIn
      , fromList <$> mbTagsIdIn)
  mapM toNews res
  where
    toTemplate Nothing  = "%"
    toTemplate (Just a) = "%" <> a <> "%"
    toNews (id, title, time, content, mainPic, addPics, categoryId, tagsId) = do
      tags <- getTags Nothing (Just $ toList tagsId) Nothing Nothing Nothing
      category <- getCategoryTreeFromBottom categoryId
      let drafts =
            tupleToDraft
              (id, title, time, content, mainPic, addPics, tags, category)
      return drafts

getDraftAuthorToken :: NewsId -> ServerStateIO [Only Token]
getDraftAuthorToken newsId = do
  conn <- gets conn
  lift $
    query
      conn
      [sql|
        SELECT user_token.token FROM user_token, author, news
        WHERE news.id = ?
        AND news.is_published = false
        AND news.author_id = author.id
        AND author.user_id = user_token.user_id
        |]
      (Only newsId)
