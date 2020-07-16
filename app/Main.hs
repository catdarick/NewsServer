{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Api.Methods.Create.Author
import           Api.Methods.Create.Category
import           Api.Methods.Create.Draft
import           Api.Methods.Create.Tag
import           Api.Methods.Delete.Author
import           Api.Methods.Delete.Category
import           Api.Methods.Delete.Comment
import           Api.Methods.Delete.Draft
import           Api.Methods.Delete.Tag
import           Api.Methods.Delete.User
import           Api.Methods.Edit.Author
import           Api.Methods.Edit.Category
import           Api.Methods.Edit.Draft
import           Api.Methods.Edit.Tag
import           Api.Methods.Get.Author
import           Api.Methods.Get.Category
import           Api.Methods.Get.News
import           Api.Methods.Get.Tag
import           Api.Methods.Get.User
import           Api.Methods.Login
import           Api.Methods.Post.Comment
import           Api.Methods.Post.Draft
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
      ["editAuthor"]     -> encoded $ editAuthor conn queryString_
      ["editCategory"]   -> encoded $ editCategory conn queryString_
      ["editTag"]        -> encoded $ editTag conn queryString_
      ["editDraft"]      -> encoded $ editDraft conn queryString_
      ["deleteUser"]     -> encoded $ deleteUser conn queryString_
      ["deleteTag"]      -> encoded $ deleteTag conn queryString_
      ["deleteDraft"]    -> encoded $ deleteDraft conn queryString_
      ["deleteComment"]  -> encoded $ deleteComment conn queryString_
      ["deleteCategory"] -> encoded $ deleteCategory conn queryString_
      ["deleteAuthor"]   -> encoded $ deleteAuthor conn queryString_
      ["getUsers"]       -> encoded $ getUsers conn queryString_
      ["getCategories"]  -> encoded $ getCategories conn queryString_
      ["getTags"]        -> encoded $ getTags conn queryString_
      ["getAuthors"]     -> encoded $ getAuthors conn queryString_
      ["getNews"]        -> encoded $ getNews conn queryString_
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
  --initDatabase conn
  print defaultConnectInfo
  run 3000 $ application conn
  return ()
