{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Database.PostgreSQL.Simple       (connectPostgreSQL,
                                                   defaultConnectInfo, execute,
                                                   execute_, query, query_)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Database.PostgreSQL.Simple.Types (Only (Only))
import           Migration.User
import           Network.HTTP.Types               (status200)
import           Network.Wai
import           Network.Wai.Handler.Warp         (run)

application request respond = do
  print request
  respond $ responseLBS status200 [("Content-Type", "text/plain")] "Hello World"

main = do
  conn <-
    connectPostgreSQL
      "host='127.0.0.1' port=5432 dbname='test' user='darick' password='IPOD103qwe'"
   -- q<-query_ conn "select somev, title from testtable"
  q <-
    query_
      conn
      [sql| WITH RECURSIVE r AS (
            SELECT id, parent_id, title
            FROM category
            WHERE id = 4
            UNION
            SELECT category.id, category.parent_id, category.title
            FROM category
                JOIN r
                    ON category.id = r.parent_id
            )
            SELECT * FROM r|]
    --createCategoryTable conn
    --execute conn "insert into category (parent, title) values (?,?)"  $  ((1::Int), ("2323"::String))
{-   execute
    conn
    [sql| insert into category
              (parent, title)
              values (?,?)
            |]
    (2::Int, "789" :: String) -}
   -- execute_ conn "create table testtable2 (somev_s INT, title_s VARCHAR)"
  print (q :: [(Int, Maybe Int, String)])
  print defaultConnectInfo
  run 3000 application
  return ()
