{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.Get.Category where

import           Api.Types.Category
import           Data.Int                         (Int64)
import           Data.Text                        (Text)
import           Database.PostgreSQL.Simple       (query_, Connection, execute, query)
import           Database.PostgreSQL.Simple       (Only (Only))
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Database.PostgreSQL.Simple.Types (Binary (Binary))
import           Database.PostgreSQL.Simple.Types (Only)
import           Database.Types



getRootCategoriesTree ::
     Connection
  -> IO [Category]
getRootCategoriesTree conn= do
  onlyRootCatgories <-
    query_
      conn
      [sql|
                SELECT id
                FROM category
                WHERE parent_id IS NULL
                |]
  let rootCatgories = map fromOnly onlyRootCatgories
  sequence $ map (getCategoryTree conn) rootCatgories 

getCategoriesTree ::
     Connection
  -> Maybe CategoryId
  -> Maybe CategoryId
  -> Maybe Name
  -> IO [Category]
getCategoriesTree conn mbId mbParentId mbName= do
  onlyRootCatgories <-
    query
      conn
      [sql|
                SELECT id
                FROM category
                WHERE id = COALESCE(?, id)
                AND parent_id = COALESCE(?, parent_id)
                AND name = COALESCE(?, name)
                |] (mbId, mbParentId, mbName)
  let rootCatgories = map fromOnly onlyRootCatgories
  sequence $ map (getCategoryTree conn) rootCatgories 

getCategoryTree :: Connection -> Int -> IO Category
getCategoryTree conn categoryId =do
    res<- query

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
    case res  of
      [(id, _, name)] -> return $ getCategory id name []
      (id, _, name):xs -> return $ getCategory id name (getCategoryChildsFromList id xs)

 
getCategoryChildsFromList :: Int -> [(Int, Maybe Int, Text)] -> [Category]
getCategoryChildsFromList p_id  xs=do
  [getCategory id name (getCategoryChildsFromList id xs)
    | (id, parent_id, name)<-xs 
    , parent_id== Just p_id]

fromOnly :: Only Int -> Int
fromOnly (Only x) = x