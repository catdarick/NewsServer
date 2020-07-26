{-# LANGUAGE OverloadedStrings #-}

module RequestHandler where

import           Api.ErrorException
import           Api.Methods.Create
import           Api.Methods.Delete
import           Api.Methods.Edit
import           Api.Methods.Get
import           Api.Methods.Create.Comment
import           Api.Methods.Create.News
import           Api.Types.Response
import           Api.Types.Synonyms
import           Config
import           Control.Monad.Catch           (MonadCatch (catch),
                                                MonadThrow (throwM))
import           Data.Aeson                    (encode)

import qualified Data.ByteString.Internal      as Strict
import qualified Data.ByteString.Lazy.Internal as Lazy
import           Data.Function                 ((&))
import           Data.Text                     (Text)
import           Database.PostgreSQL.Simple    (Connection)
import qualified Network.HTTP.Types.Method     as Wai
import           Network.HTTP.Types.Status
import qualified Network.Wai                   as Wai

encoded (status, response) = (status, encode response)

callGetMethod ::
     Connection
  -> Config
  -> [(FieldName, Maybe Strict.ByteString)]
  -> [Text]
  -> IO (Status, Lazy.ByteString)
callGetMethod conn config query path =
  case path of
    ["getUsers"]      -> encoded <$> getUsers conn query
    ["getCategories"] -> encoded <$> getCategories conn query
    ["getTags"]       -> encoded <$> getTags conn query
    ["getAuthors"]    -> encoded <$> getAuthors conn query
    ["getNews"]       -> encoded <$> getNews conn query
    ["getDrafts"]     -> encoded <$> getDrafts conn query
    ["getComments"]   -> encoded <$> getComments conn query
    smth              -> return (status404, "")

callPostMethod ::
     Connection
  -> Config
  -> [(FieldName, Maybe Strict.ByteString)]
  -> [Text]
  -> IO (Status, Lazy.ByteString)
callPostMethod conn config query path =
  case path of
    ["createAccount"]  -> encoded <$> createAccount conn config query
    ["createAuthor"]   -> encoded <$> createAuthor conn query
    ["createCategory"] -> encoded <$> createCategory conn query
    ["createTag"]      -> encoded <$> createTag conn query
    ["createDraft"]    -> encoded <$> createDraft conn query
    ["createComment"]    -> encoded <$> createComment conn query
    smth               -> return (status404, "")

callPutMethod ::
     Connection
  -> Config
  -> [(FieldName, Maybe Strict.ByteString)]
  -> [Text]
  -> IO (Status, Lazy.ByteString)
callPutMethod conn config query path =
  case path of
    ["login"]        -> encoded <$> getToken conn query
    ["publishDraft"] -> encoded <$> publishDraft conn query
    ["editAuthor"]   -> encoded <$> editAuthor conn query
    ["editCategory"] -> encoded <$> editCategory conn query
    ["editTag"]      -> encoded <$> editTag conn query
    ["editDraft"]    -> encoded <$> editDraft conn query
    smth             -> return (status404, "")

callDeleteMethod ::
     Connection
  -> Config
  -> [(FieldName, Maybe Strict.ByteString)]
  -> [Text]
  -> IO (Status, Lazy.ByteString)
callDeleteMethod conn config query path =
  case path of
    ["deleteUser"]     -> encoded <$> deleteUser conn query
    ["deleteTag"]      -> encoded <$> deleteTag conn query
    ["deleteDraft"]    -> encoded <$> deleteDraft conn query
    ["deleteNews"]     -> encoded <$> deleteNews conn query
    ["deleteComment"]  -> encoded <$> deleteComment conn query
    ["deleteCategory"] -> encoded <$> deleteCategory conn query
    ["deleteAuthor"]   -> encoded <$> deleteAuthor conn query
    smth               -> return (status404, "")

callMethod ::
     Connection
  -> Config
  -> [(FieldName, Maybe Strict.ByteString)]
  -> [Text]
  -> Wai.Method
  -> IO (Status, Lazy.ByteString)
callMethod conn config query path method =
  case method of
    "GET"    -> callGetMethod conn config query path
    "POST"   -> callPostMethod conn config query path
    "DELETE" -> callDeleteMethod conn config query path
    smth     -> return (status404, "")

handleRequest ::
     Connection -> Config -> Wai.Request -> (Wai.Response -> IO b) -> IO b
handleRequest conn config request respond = do
  let query = request & Wai.queryString
  let path = request & Wai.pathInfo
  let method = request & Wai.requestMethod
  (status, bsResponse) <-
    catch (callMethod conn config query path method) errorHandler
  respond $
    Wai.responseLBS status [("Content-Type", "application/json")] bsResponse
  where
    errorHandler (ErrorException status error) =
      return (status, encode $ errorResponse error)
