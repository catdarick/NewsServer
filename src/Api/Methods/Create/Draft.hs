{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Create.Draft where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import qualified Api.Methods.Errors               as Err
import           Api.Types
import           Api.Types.Response
import           Data.ByteString                  (ByteString)
import qualified Database.Create.Draft            as DB
import qualified Database.Get.Author              as DB
import           Database.PostgreSQL.Simple       (Connection)
import           Database.PostgreSQL.Simple.Types (Only (Only))
import           Network.HTTP.Types               (status200, Status, status400)

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
      --print maybeAuthorId
      case maybeAuthorId of
        [] -> return (status400, errorResponse Err.notAuthor)
        [Only authorId] -> do
          res <-
            DB.addDraftWithTags
              conn
              authorId
              title
              content
              (toInt categoryId)
              mainPicture
              (toStringList <$> pictures)
              (toIntList <$> tagsId)
          case res of
            Left err        -> return (status400, errorResponse err)
            Right [Only id] -> return (status200, idResponse id)
  where
    requiredNames = ["token", "title", "content", "category_id"]
    requiredChecks =
      [isNotEmpty, isCorrectLength 5 50, isCorrectLength 5 10000, isInt]
    required = (requiredNames, requiredChecks)
    optionalNames = ["tags_id", "main_picture", "pictures"]
    optionalChecks = [isIntList, isNotEmpty, isNotEmptyTextList]
    optional = (optionalNames, optionalChecks)
