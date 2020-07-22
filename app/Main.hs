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
import           Api.ErrorException
import Control.Monad.Catch (MonadThrow(throwM))
import Control.Monad.Catch (MonadCatch(catch))
import Data.ByteString (ByteString)
callMethod conn config query path =  do
  res <- case path of
      ["createAccount"]  -> encode <$> createAccount conn config query
      -- ["logIn"]          -> encode <$> getToken conn query
      ["createAuthor"]   -> encode <$> createAuthor conn query
      ["createCategory"] -> encode <$> createCategory conn query
      ["createTag"]      -> encode <$> createTag conn query
      ["createDraft"]    -> encode <$> createDraft conn query
      -- ["postDraft"]      -> encode <$> postDraft conn queryString_
      -- ["postComment"]    -> encode $ postComment conn queryString_
      -- ["editAuthor"]     -> encode $ editAuthor conn queryString_
      -- ["editCategory"]   -> encoded $ editCategory conn queryString_
      -- ["editTag"]        -> encoded $ editTag conn queryString_
      -- ["editDraft"]      -> encoded $ editDraft conn queryString_
      ["deleteUser"]     -> encode <$> deleteUser conn query
      -- ["deleteTag"]      -> encoded $ deleteTag conn queryString_
      -- ["deleteDraft"]    -> encoded $ deleteDraft conn queryString_
      -- ["deleteComment"]  -> encoded $ deleteComment conn queryString_
      -- ["deleteCategory"] -> encoded $ deleteCategory conn queryString_
      ["deleteAuthor"]   -> encode <$> deleteAuthor conn query
      -- ["getUsers"]       -> encoded $ getUsers conn queryString_
      -- ["getCategories"]  -> encoded $ getCategories conn queryString_
      -- ["getTags"]        -> encoded $ getTags conn queryString_
      -- ["getAuthors"]     -> encoded $ getAuthors conn queryString_
      -- ["getNews"]        -> encoded $ getNews conn queryString_
      -- ["getDrafts"]      -> encoded $ getDrafts conn queryString_
      -- ["getComments"]    -> encoded $ getComments conn queryString_
      smth               -> throwM $ ErrorException status404 ""
  return (status200, res)


application conn config request respond = do
  let query = request & queryString
  let path = request & pathInfo
  (status, bsResponse) <- catch (callMethod conn config query path) errorHandler
  respond $
    responseLBS status [("Content-Type", "application/json")] $ bsResponse
  where
    errorHandler (ErrorException status error) = return $ (status, encode $ errorResponse2 error)

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
