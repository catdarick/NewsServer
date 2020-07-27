{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.Get.Category where

import           Api.Types.Category
import           Api.Types.Synonyms
import           Control.Monad.Trans.Class        (MonadTrans (lift))
import           Control.Monad.Trans.State        (gets)
import           Data.List                        (find)
import           Data.Maybe                       (fromJust, isNothing)
import           Data.Text                        (Text)
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   query, query_)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           State.Types

getRootCategoriesTree :: ServerStateIO [Category]
getRootCategoriesTree = do
  conn <- gets conn
  onlyRootCatgories <-
    lift $
    query_
      conn
      [sql|
                SELECT id
                FROM category
                WHERE parent_id IS NULL
                |]
  let rootCatgories = map fromOnly onlyRootCatgories
  mapM getCategoryTreeFromTop rootCatgories

getCategoriesTreeFromTop ::
     Maybe CategoryId
  -> Maybe CategoryId
  -> Maybe Name
  -> ServerStateIO [Category]
getCategoriesTreeFromTop mbId mbParentId mbName = do
  conn <- gets conn
  onlyRootCatgories <-
    lift $
    query
      conn
      [sql|
       SELECT id
       FROM category
       WHERE id = COALESCE(?, id)
       AND (parent_id IS NULL OR parent_id = COALESCE(?, parent_id))
       AND name = COALESCE(?, name)
                |]
      (mbId, mbParentId, mbName)
  let rootCatgories = map fromOnly onlyRootCatgories
  mapM getCategoryTreeFromTop rootCatgories

getCategoryTreeFromTop :: Int -> ServerStateIO Category
getCategoryTreeFromTop categoryId = do
  conn <- gets conn
  res <-
    lift $
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

getCategoryTreeFromBottom :: Int -> ServerStateIO Category
getCategoryTreeFromBottom categoryId = do
  conn <- gets conn
  res <-
    lift $
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
