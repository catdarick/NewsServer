{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.Get.Draft where

import           Api.Types.Author
import           Api.Types.News
import           Data.Int                         (Int64)
import           Data.Text                        (Text)
import           Data.Vector                      (fromList, toList)
import           Database.Get.Category
import           Database.Get.Comment
import           Database.Get.Tag
import           Database.PostgreSQL.Simple       (Connection, execute, query,
                                                   query_)
import           Database.PostgreSQL.Simple       (Only (Only))
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Database.PostgreSQL.Simple.Types (Binary (Binary))
import           Database.PostgreSQL.Simple.Types (Only)
import           Database.Types

getDrafts ::
     Connection
  -> Token
  -> Maybe CategoryId
  -> Maybe TagId
  -> Maybe [TagId]
  -> Maybe [TagId]
  -> Maybe Title
  -> Maybe Content
  -> Maybe Limit
  -> Maybe Offset
  -> IO [News]
getDrafts conn token mbCategotyId mbTagId mbTagsIdIn mbTagsIdAll mbTitle mbContent mbLimit mbOffset = do
  res <-
    query
      conn
      [sql|
                SELECT * FROM (SELECT news.id, news.title, date_trunc('second',news.creation_time), news.content, news.main_picture, news.pictures, news.category_id,
                (SELECT ARRAY(select news_tag.tag_id from news_tag
                  where news_tag.news_id = news.id)) as tagsId
                FROM news, author, user_token
                WHERE news.is_published = false
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
      ( toTemplate mbTitle
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
  sequence $ map f res
  where
    toTemplate Nothing  = "%"
    toTemplate (Just a) = "%" <> a <> "%"
    f (id, title, time, content, mainPic, addPics, categoryId, tagsId) = do
      tags <-
        getTags conn Nothing (Just $ toList tagsId) Nothing Nothing Nothing
      category <- getCategoryTreeFromBottom conn categoryId
      let drafts =
            tupleToDraft
              ( id
              , title
              , time
              , content
              , mainPic
              , addPics
              , tags
              , category)
      return drafts
