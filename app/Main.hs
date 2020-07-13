{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Database.PostgreSQL.Simple       (connectPostgreSQL,
                                                   defaultConnectInfo, execute,
                                                   execute_, query, query_)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Database.PostgreSQL.Simple.Types (Only (Only))
import           Migration.Create
import           Network.HTTP.Types               (status200)
import           Network.Wai
import           Network.Wai.Handler.Warp         (run)
import Control.Monad (void)
import Api.Methods.Signin
import Api.Methods.Login
import Data.Function ((&))
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types.Status 
application conn request respond = do
  print request
  let queryString_ = (request & queryString)
  let path = request & pathInfo
  (status, response) <- case path of
    ["signIn"] -> signIn conn queryString_
    ["logIn"] -> logIn conn queryString_
    smth -> return (status404, "")
  respond $ responseLBS status [("Content-Type", "application/json")] response

main = do
  conn <-
    connectPostgreSQL
      "host='127.0.0.1' port=5432 dbname='test' user='darick' password='IPOD103qwe'"
   -- q<-query_ conn "select somev, title from testtable"
  --initDatabase conn
  --createNewsTable conn
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
  --print (q :: [(Int, Maybe Int, String)])
  print defaultConnectInfo
  run 3000 $ application conn
  return ()
