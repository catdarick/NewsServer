{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Api.Methods.CreateAuthor
import           Api.Methods.CreateCategory
import           Api.Methods.CreateDraft
import           Api.Methods.CreateTag
import           Api.Methods.Login
import           Api.Methods.PostComment
import           Api.Methods.PostDraft
import           Api.Methods.Signin
import           Api.Types.Response
import           Control.Monad                    (void, when)
import           Data.Aeson                       (encode)
import           Data.Function                    ((&))
import           Data.Text.Encoding               (decodeUtf8)
import           Database.PostgreSQL.Simple       (connectPostgreSQL,
                                                   defaultConnectInfo, execute,
                                                   execute_, query, query_)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Database.PostgreSQL.Simple.Types (Only (Only))
import           Migration.Create
import           Network.HTTP.Types               (status200)
import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Handler.Warp         (run)

application conn request respond = do
  print request
  let queryString_ = (request & queryString)
  let path = request & pathInfo
  (status, bsResponse) <-
    case path of
      ["signIn"]         -> encoded $ signIn conn queryString_
      ["logIn"]          -> encoded $ logIn conn queryString_
      ["createAuthor"]   -> encoded $ createAuthor conn queryString_
      ["createCategory"] -> encoded $ createCategory conn queryString_
      ["createTag"]      -> encoded $ createTag conn queryString_
      ["createDraft"]    -> encoded $ createDraft conn queryString_
      ["postDraft"]      -> encoded $ postDraft conn queryString_
      ["postComment"]    -> encoded $ postComment conn queryString_
      smth               -> return (status404, "")
  respond $
    responseLBS status [("Content-Type", "application/json")] $ bsResponse
    --if (status == status404)
    --  then ""
    --  else bsResponse
  where
    encoded f = do
      (status, response) <- f
      return (status, encode response)

main = do
  conn <-
    connectPostgreSQL
      "host='127.0.0.1' port=5432 dbname='test' user='darick' password='IPOD103qwe'"
  print defaultConnectInfo
  run 3000 $ application conn
  return ()
