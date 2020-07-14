{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.PostNews where

import           Api.Helpers.Check
import           Api.Helpers.Getters
import qualified Api.Methods.Errors               as Err
import           Api.Types.Response
import           Control.Exception                (SomeException, try)
import           Data.ByteString                  (ByteString)
import qualified Database.Author                  as DB
import qualified Database.News                    as DB
import           Database.PostgreSQL.Simple       (SqlError, sqlErrorMsg, Connection)
import           Database.PostgreSQL.Simple.Types (Only (Only))
import           Database.Types
import           Network.HTTP.Types               (Status, status400)

postNews ::
     Connection -> [(ByteString, Maybe Login)] -> IO (Status, Response ())
postNews conn queryString = do
  let eitherParameters = checkAndGetParameters required optional queryString
  case eitherParameters of
    Left error -> return (status400, errorResponse error)
    Right (requiredValues, optionalMaybeValues) -> do
      let [token, title, content, categoryId] = requiredValues
      let [tagsId, mainPicture, pictures] = optionalMaybeValues
      maybeAuthorId <- DB.getAuthorId conn token
      print maybeAuthorId
      case maybeAuthorId of
        [] -> return (status400, errorResponse Err.notAuthor)
        [Only authorId] -> do
          res <-
            try $
            DB.addNews
              conn
              authorId
              title
              content
              (fromInt categoryId)
              mainPicture
              (fromStringList <$> pictures)
          case res of
            Left (e :: SqlError) ->do
              print e
              return (status400, errorResponse Err.noCategory)
            Right _ -> return (status400, okResponse)
  where
    requiredNames = ["token", "title", "content", "category_id"]
    requiredChecks =
      [isNotEmpty, isCorrectLength 5 50, isCorrectLength 5 10000, isInt]
    required = (requiredNames, requiredChecks)
    optionalNames = ["tags_id", "main_picture", "pictures"]
    optionalChecks = [isIntList, isNotEmpty, isNotEmptyTextList]
    optional = (optionalNames, optionalChecks)
