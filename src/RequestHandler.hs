{-# LANGUAGE OverloadedStrings #-}

module RequestHandler where

import           Api.ErrorException
import           Api.Methods.Create
import           Api.Methods.Create.Comment
import           Api.Methods.Create.News
import           Api.Methods.Delete
import           Api.Methods.Edit
import           Api.Methods.Get
import           Api.Types.Response
import           Api.Types.Synonyms
import           Config
import           Control.Monad.Catch           (MonadCatch (catch),
                                                MonadThrow (throwM))
import           Control.Monad.Trans.Class     (MonadTrans (lift))
import           Control.Monad.Trans.State     (evalStateT)
import           Data.Aeson                    (ToJSON, encode)
import qualified Data.ByteString.Internal      as Strict
import qualified Data.ByteString.Lazy.Internal as Lazy
import           Data.Function                 ((&))
import           Data.Text                     (Text)
import           Database.PostgreSQL.Simple    (Connection)
import qualified Logger.Interact               as Log
import qualified Network.HTTP.Types.Method     as Wai
import           Network.HTTP.Types.Status
import qualified Network.Wai                   as Wai
import           State.Types

encoded :: ToJSON a1 => (a2, a1) -> (a2, Lazy.ByteString)
encoded (status, response) = (status, encode response)

callGetMethod ::
     [(FieldName, Maybe Strict.ByteString)]
  -> [Text]
  -> ServerStateIO (Status, Lazy.ByteString)
callGetMethod query path =
  case path of
    ["getUsers"]      -> encoded <$> getUsers query
    ["getCategories"] -> encoded <$> getCategories query
    ["getTags"]       -> encoded <$> getTags query
    ["getAuthors"]    -> encoded <$> getAuthors query
    ["getNews"]       -> encoded <$> getNews query
    ["getDrafts"]     -> encoded <$> getDrafts query
    ["getComments"]   -> encoded <$> getComments query
    smth              -> return (status404, "")

callPostMethod ::
     [(FieldName, Maybe Strict.ByteString)]
  -> [Text]
  -> ServerStateIO (Status, Lazy.ByteString)
callPostMethod query path =
  case path of
    ["createAccount"]  -> encoded <$> createAccount query
    ["createAuthor"]   -> encoded <$> createAuthor query
    ["createCategory"] -> encoded <$> createCategory query
    ["createTag"]      -> encoded <$> createTag query
    ["createDraft"]    -> encoded <$> createDraft query
    ["createComment"]  -> encoded <$> createComment query
    smth               -> return (status404, "")

callPutMethod ::
     [(FieldName, Maybe Strict.ByteString)]
  -> [Text]
  -> ServerStateIO (Status, Lazy.ByteString)
callPutMethod query path =
  case path of
    ["getToken"]     -> encoded <$> getToken query
    ["publishDraft"] -> encoded <$> publishDraft query
    ["editAuthor"]   -> encoded <$> editAuthor query
    ["editCategory"] -> encoded <$> editCategory query
    ["editTag"]      -> encoded <$> editTag query
    ["editDraft"]    -> encoded <$> editDraft query
    smth             -> return (status404, "")

callDeleteMethod ::
     [(FieldName, Maybe Strict.ByteString)]
  -> [Text]
  -> ServerStateIO (Status, Lazy.ByteString)
callDeleteMethod query path =
  case path of
    ["deleteUser"]     -> encoded <$> deleteUser query
    ["deleteTag"]      -> encoded <$> deleteTag query
    ["deleteDraft"]    -> encoded <$> deleteDraft query
    ["deleteNews"]     -> encoded <$> deleteNews query
    ["deleteComment"]  -> encoded <$> deleteComment query
    ["deleteCategory"] -> encoded <$> deleteCategory query
    ["deleteAuthor"]   -> encoded <$> deleteAuthor query
    smth               -> return (status404, "")

callMethod ::
     [(FieldName, Maybe Strict.ByteString)]
  -> [Text]
  -> Wai.Method
  -> ServerStateIO (Status, Lazy.ByteString)
callMethod query path method =
  case method of
    "GET"    -> callGetMethod query path
    "POST"   -> callPostMethod query path
    "PUT"    -> callPutMethod query path
    "DELETE" -> callDeleteMethod query path
    smth     -> return (status404, "")

handleRequest ::
     Connection -> Config -> Wai.Request -> (Wai.Response -> IO b) -> IO b
handleRequest conn config request respond = do
  let query = request & Wai.queryString
  let path = request & Wai.pathInfo
  let rawPath = request & Wai.rawPathInfo
  let method = request & Wai.requestMethod
  let clientInfo = request & Wai.remoteHost
  let state = ServerState config conn clientInfo
  (status, bsResponse) <-
    evalStateT
      (catch (callMethod query path method) (errorHandler rawPath))
      state
  respond $
    Wai.responseLBS status [("Content-Type", "application/json")] bsResponse
  where
    callMethodWithState state query path method =
      evalStateT (callMethod query path method) state
    errorHandler path (ErrorException status error) = do
      Log.debug $ "Error response from " <> path <> ": " <> error
      return (status, encode $ errorResponse error)
