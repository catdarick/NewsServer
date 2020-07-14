{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.CreateDraft where

import           Api.Helpers.Check
import           Api.Helpers.Getters
import qualified Api.Methods.Errors               as Err
import           Api.Types.Response
import           Control.Exception                (SomeException, try)
import           Data.ByteString                  (ByteString)
import qualified Database.Author                  as DB
import qualified Database.Draft                    as DB
import           Database.PostgreSQL.Simple       (Connection, SqlError,
                                                   sqlErrorMsg)
import           Database.PostgreSQL.Simple.Types (Only (Only))
import           Database.Types
import           Network.HTTP.Types               (Status, status400)

createDraft ::
     Connection -> [(ByteString, Maybe Login)] -> IO (Status, Response Idcont)
createDraft conn queryString = do
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
            DB.addDraftWithTags
              conn
              authorId
              title
              content
              (fromInt categoryId)
              mainPicture
              (fromStringList <$> pictures)
              (fromIntList <$> tagsId)
          case res of
            Left err -> return (status400, errorResponse err)
            Right [Only id] -> do
              return (status400, idResponse id)
  where
    requiredNames = ["token", "title", "content", "category_id"]
    requiredChecks =
      [isNotEmpty, isCorrectLength 5 50, isCorrectLength 5 10000, isInt]
    required = (requiredNames, requiredChecks)
    optionalNames = ["tags_id", "main_picture", "pictures"]
    optionalChecks = [isIntList, isNotEmpty, isNotEmptyTextList]
    optional = (optionalNames, optionalChecks)
