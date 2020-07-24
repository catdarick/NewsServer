{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Create.Draft where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import           Api.Types.Response
import           Api.Types.Synonyms
import           Data.ByteString            (ByteString)
import qualified Database.Create.Draft      as DB
import qualified Database.Get.Author        as DB
import           Database.PostgreSQL.Simple (Connection)

createDraft :: Connection -> [(ByteString, Maybe Login)] -> IO (Response Idcont)
createDraft conn queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
  let [token, title, content, categoryId] = requiredValues
  let [tagsId, mainPicture, pictures] = optionalMaybeValues
  authorId <- DB.getAuthorIdOrThrow conn token
  id <-
    DB.addDraftWithTags
      conn
      authorId
      title
      content
      (toInt categoryId)
      mainPicture
      (toStringList <$> pictures)
      (toIntList <$> tagsId)
  return $ idResponse id
  where
    requiredNames = ["token", "title", "content", "category_id"]
    requiredChecks =
      [isNotEmpty, isCorrectLength 5 50, isCorrectLength 5 10000, isInt]
    required = (requiredNames, requiredChecks)
    optionalNames = ["tags_id", "main_picture", "pictures"]
    optionalChecks = [isIntList, isNotEmpty, isNotEmptyTextList]
    optional = (optionalNames, optionalChecks)
    parameters = checkAndGetParameters required optional queryString
