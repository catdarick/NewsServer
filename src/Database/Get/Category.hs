{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.Get.Category where

import           Api.Types
import           Api.Types.Category
import           Data.List                        (find)
import           Data.Maybe                       (fromJust, isNothing)
import           Data.Text                        (Text)
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   query, query_)
import           Database.PostgreSQL.Simple.SqlQQ (sql)

getRootCategoriesTree :: Connection -> IO [Category]
getRootCategoriesTree conn = do
  onlyRootCatgories <-
    query_
      conn
      [sql|
                SELECT id
                FROM category
                WHERE parent_id IS NULL
                |]
  let rootCatgories = map fromOnly onlyRootCatgories
  mapM (getCategoryTreeFromTop conn) rootCatgories

getCategoriesTreeFromTop ::
     Connection
  -> Maybe CategoryId
  -> Maybe CategoryId
  -> Maybe Name
  -> IO [Category]
getCategoriesTreeFromTop conn mbId mbParentId mbName = do
  onlyRootCatgories <-
    query
      conn
      [sql|
                SELECT id
                FROM category
                WHERE id = COALESCE(?, id)
                AND parent_id = COALESCE(?, parent_id)
                AND name = COALESCE(?, name)
                |]
      (mbId, mbParentId, mbName)
  let rootCatgories = map fromOnly onlyRootCatgories
  sequence $ map (getCategoryTreeFromTop conn) rootCatgories

getCategoryTreeFromTop :: Connection -> Int -> IO Category
getCategoryTreeFromTop conn categoryId = do
  res <-
    query
      conn
      [sql| WITH RECURSIVE r AS (
              SELECT id, parent_id, name
              FROM category
              WHERE id =  ?
              UNION
              SELECT category.id, category.parent_id, category.name
              FROM category
                  JOIN r
                      ON category.parent_id = r.id
              )
              SELECT * FROM r|]
      (Only categoryId)
  case res of
    [(id, _, name)] -> return $ getCategory id name []
    (id, _, name):xs ->
      return $ getCategory id name (getCategoryChildsFromList id xs)

getCategoryTreeFromBottom :: Connection -> Int -> IO Category
getCategoryTreeFromBottom conn categoryId = do
  res <-
    query
      conn
      [sql| WITH RECURSIVE r AS (
              SELECT id, parent_id, name
              FROM category
              WHERE id =  ?
              UNION
              SELECT category.id, category.parent_id, category.name
              FROM category
                  JOIN r
                      ON category.id = r.parent_id
              )
              SELECT * FROM r|]
      (Only categoryId)
  let (id, _, name) = getRoot res
  case res of
    [(id, _, name)] -> return $ getCategory id name []
    xs -> return $ getCategory id name (getCategoryChildsFromList id xs)
  where
    getRoot xs = fromJust $ find isRoot xs
    isRoot (_, parent_id, _) = isNothing parent_id

getCategoryChildsFromList :: Int -> [(Int, Maybe Int, Text)] -> [Category]
getCategoryChildsFromList p_id xs =
  [ getCategory id name (getCategoryChildsFromList id xs)
  | (id, parent_id, name) <- xs
  , parent_id == Just p_id
  ]

fromOnly :: Only Int -> Int
fromOnly (Only x) = x
