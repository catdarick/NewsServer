{-# LANGUAGE OverloadedStrings #-}

module RequestHandler where

import           Api.ErrorException
import           Api.Methods.Create
import           Api.Methods.Delete
import           Api.Methods.Edit
import           Api.Methods.Get
import           Api.Methods.Post.Comment
import           Api.Methods.Post.Draft
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
import           Network.HTTP.Types.Status
import qualified Network.Wai                   as Wai

callMethod ::
     Connection
  -> Config
  -> [(FieldName, Maybe Strict.ByteString)]
  -> [Text]
  -> IO (Status, Lazy.ByteString)
callMethod conn config query path = do
  res <-
    case path of
      ["createAccount"]  -> encode <$> createAccount conn config query
      ["logIn"]          -> encode <$> getToken conn query
      ["createAuthor"]   -> encode <$> createAuthor conn query
      ["createCategory"] -> encode <$> createCategory conn query
      ["createTag"]      -> encode <$> createTag conn query
      ["createDraft"]    -> encode <$> createDraft conn query
      ["postDraft"]      -> encode <$> postDraft conn query
      ["postComment"]    -> encode <$> postComment conn query
      ["editAuthor"]     -> encode <$> editAuthor conn query
      ["editCategory"]   -> encode <$> editCategory conn query
      ["editTag"]        -> encode <$> editTag conn query
      ["editDraft"]      -> encode <$> editDraft conn query
      ["deleteUser"]     -> encode <$> deleteUser conn query
      ["deleteTag"]      -> encode <$> deleteTag conn query
      ["deleteDraft"]    -> encode <$> deleteDraft conn query
      ["deleteNews"]     -> encode <$> deleteNews conn query
      ["deleteComment"]  -> encode <$> deleteComment conn query
      ["deleteCategory"] -> encode <$> deleteCategory conn query
      ["deleteAuthor"]   -> encode <$> deleteAuthor conn query
      ["getUsers"]       -> encode <$> getUsers conn query
      ["getCategories"]  -> encode <$> getCategories conn query
      ["getTags"]        -> encode <$> getTags conn query
      ["getAuthors"]     -> encode <$> getAuthors conn query
      ["getNews"]        -> encode <$> getNews conn query
      ["getDrafts"]      -> encode <$> getDrafts conn query
      ["getComments"]    -> encode <$> getComments conn query
      smth               -> throwM $ ErrorException status404 ""
  return (status200, res)

handleRequest ::
     Connection -> Config -> Wai.Request -> (Wai.Response -> IO b) -> IO b
handleRequest conn config request respond = do
  let query = request & Wai.queryString
  let path = request & Wai.pathInfo
  (status, bsResponse) <- catch (callMethod conn config query path) errorHandler
  respond $
    Wai.responseLBS status [("Content-Type", "application/json")] bsResponse
  where
    errorHandler (ErrorException status error) =
      return (status, encode $ errorResponse2 error)
