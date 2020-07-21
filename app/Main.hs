{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Api.Methods.Create
import           Api.Methods.Delete
import           Api.Methods.Edit
import           Api.Methods.Get
import           Api.Methods.Post.Comment
import           Api.Methods.Post.Draft
import           Api.Types.Response
import           Control.Monad              (void, when)
import           Data.Aeson                 (encode)
import           Data.Function              ((&))
import           Data.Text.Encoding         (decodeUtf8)
import           Database.PostgreSQL.Simple (connectPostgreSQL,
                                             defaultConnectInfo)
import           Migration.Create
import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Handler.Warp   (run)
import Control.Exception (try, SomeException)
import Data.Configurator.Types (Worth(Required))
import Data.Configurator (load)
import Config
import State.Types
import           Data.Function               ((&))
import Control.Monad.Trans.State (StateT(runStateT))
import Control.Monad.Trans.Class (MonadTrans(lift))
application conn config request respond = do
  --print request
  let queryString_ = request & queryString
  let path = request & pathInfo
  (status, bsResponse) <-
    case path of
      ["createAccount"]  -> encoded $ createAccount conn config queryString_
      ["logIn"]          -> encoded $ getToken conn queryString_
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
      ["getDrafts"]      -> encoded $ getDrafts conn queryString_
      ["getComments"]    -> encoded $ getComments conn queryString_
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
  eitherCfg <- try $ load [Required "$(PWD)/app/server.cfg"]
  case eitherCfg of
    Left (e :: SomeException) -> print "Can't find config file" >> print e
    Right handleConfig -> do
      config <- parseConfig handleConfig
      
      conn <-
        connectPostgreSQL $ connectString config

      run 3000 $ (application conn config)  
      
  
  
  --initDatabase conn

  
  return ()
  where 
    connectString config="host='" <> (config & dbHost) <> "' port=" <> (config & dbPort) <> 
        " dbname='" <> (config & dbName) <> "' user='" <> (config&dbUsername) <> "' password='" 
        <> (config&dbUserPass) <> "'" 
