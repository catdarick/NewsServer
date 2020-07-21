{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Database.Get.News where

import           Api.Types
import           Api.Types.Author
import           Api.Types.News
import           Data.Time                        (LocalTime)
import           Data.Vector                      (fromList, toList)
import           Database.Get.Category
import           Database.Get.Comment
import           Database.Get.Tag
import           Database.PostgreSQL.Simple       (Connection, query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)

getNews ::
     Connection
  -> Maybe AuthorId
  -> Maybe Login
  -> Maybe FirstName
  -> Maybe LastName
  -> Maybe CategoryId
  -> Maybe TagId
  -> Maybe [TagId]
  -> Maybe [TagId]
  -> Maybe Title
  -> Maybe Content
  -> Maybe Int
  -> Maybe LocalTime
  -> Maybe LocalTime
  -> Maybe LocalTime
  -> Maybe Limit
  -> Maybe Offset
  -> IO [News]
getNews conn mbAuthorId mbLogin mbFName mbLName mbCategotyId mbTagId mbTagsIdIn mbTagsIdAll mbTitle mbContent mbSort mbDate mbDateBefore mbDateAfter mbLimit mbOffset = do
  res <-
    query
      conn
      [sql|
                SELECT news_id, title, news_creation_time,content,main_picture,pictures,category_id,tags_id,author_id,description,user_id,login,first_name,last_name,picture,user_creation_time,is_admin
                FROM (SELECT (?::int) as sort, news.id as news_id, news.title, date_trunc('second',news.creation_time) as news_creation_time, news.content, news.main_picture, news.pictures, news.category_id,
                (SELECT ARRAY(select news_tag.tag_id from news_tag
                  WHERE news_tag.news_id = news.id)) as tags_id,
                author.id as author_id, author.description,
                usr.id as user_id, usr.login, usr.first_name, usr.last_name, usr.picture, date_trunc('second',usr.creation_time) as user_creation_time, usr.is_admin,
                category.name as category_name
                FROM news, user_account as usr, author, category
                WHERE news.is_published = true
                AND news.author_id = author.id
                AND news.title LIKE ?
                AND news.content LIKE ?
                AND author.user_id = usr.id
                AND author.id = COALESCE(?, author.id)
                AND usr.login = COALESCE(?, usr.login)
                AND usr.first_name = COALESCE(?, usr.first_name)
                AND usr.last_name = COALESCE(?, usr.last_name)
                AND news.category_id = COALESCE(?, news.category_id)
                AND news.category_id = category.id
                AND date_trunc('day',news.creation_time) =  COALESCE(?, date_trunc('day',news.creation_time))
                AND date_trunc('day',news.creation_time) <=  COALESCE(?, date_trunc('day',news.creation_time))
                AND date_trunc('day',news.creation_time) >=  COALESCE(?, date_trunc('day',news.creation_time))
                LIMIT COALESCE(?, 50)
                OFFSET COALESCE(?, 0)) as foo
                WHERE  (? IS NULL OR (? = ANY (foo.tags_id)))
                AND (? IS NULL OR (? <@ (foo.tags_id)))
                AND (? IS NULL OR (? && (foo.tags_id)))
                ORDER BY CASE WHEN (foo.sort IS NULL) OR (foo.sort = 1)  THEN news_creation_time END DESC,
                         CASE WHEN foo.sort = 2 THEN news_creation_time END ASC,
                         CASE WHEN foo.sort = 3 THEN first_name END, last_name DESC,
                         CASE WHEN foo.sort = 4 THEN first_name END, last_name ASC,
                         CASE WHEN foo.sort = 5 THEN category_name END DESC,
                         CASE WHEN foo.sort = 6 THEN category_name END ASC,
                         CASE WHEN foo.sort = 7 THEN array_length(pictures,1) END DESC,
                         CASE WHEN foo.sort = 8 THEN array_length(pictures,1) END ASC
                |]
      ( mbSort
      , toTemplate mbTitle
      , toTemplate mbContent
      , mbAuthorId
      , mbLogin
      , mbFName
      , mbLName
      , mbCategotyId
      , mbDate
      , mbDateBefore
      , mbDateAfter
      , mbLimit
      , mbOffset
      , mbTagId
      , mbTagId
      , fromList <$> mbTagsIdAll
      , fromList <$> mbTagsIdAll
      , fromList <$> mbTagsIdIn
      , fromList <$> mbTagsIdIn)
  mapM f res
  where
    toTemplate Nothing  = "%"
    toTemplate (Just a) = "%" <> a <> "%"
    f (id, title, time, content, mainPic, addPics, categoryId, tagsId, authorId, mbDescr, userId, login, fName, lName, userPic, userCrTime, userIsAdmin) = do
      tags <-
        getTags conn Nothing (Just $ toList tagsId) Nothing Nothing Nothing
      category <- getCategoryTreeFromBottom conn categoryId
      let news =
            tupleToNews
              ( id
              , title
              , time
              , content
              , mainPic
              , addPics
              , tags
              , category
              , authorId
              , mbDescr
              , userId
              , login
              , fName
              , lName
              , userPic
              , userCrTime
              , userIsAdmin)
      return news
