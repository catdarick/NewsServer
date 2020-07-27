{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Create.Draft where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import           Api.Types.Response
import           Api.Types.Synonyms
import           Data.ByteString            (ByteString)
import           Data.ByteString.Char8      (pack)
import qualified Database.Create.Draft      as DB
import qualified Database.Get.Author        as DB
import           Database.PostgreSQL.Simple (Connection)
import qualified Logger.Interact            as Log
import           Network.HTTP.Types         (Status, status201)
import           State.Types

createDraft ::
     [(ByteString, Maybe Login)] -> ServerStateIO (Status, Response Idcont)
createDraft queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
  let [token, title, content, categoryId] = requiredValues
  let [tagsId, mainPicture, pictures] = optionalMaybeValues
  authorId <- DB.getAuthorIdOrThrow token
  id <-
    DB.addDraftWithTags
      authorId
      title
      content
      (toInt categoryId)
      mainPicture
      (toStringList <$> pictures)
      (toIntList <$> tagsId)
  Log.info $
    "Draft by author  '" <> pack (show authorId) <> "' successfully created"
  return (status201, idResponse id)
  where
    requiredNames = ["token", "title", "content", "category_id"]
    requiredChecks =
      [isNotEmpty, isCorrectLength 5 50, isCorrectLength 5 10000, isInt]
    required = (requiredNames, requiredChecks)
    optionalNames = ["tags_id", "main_picture", "pictures"]
    optionalChecks = [isIntList, isNotEmpty, isNotEmptyTextList]
    optional = (optionalNames, optionalChecks)
    parameters = checkAndGetParameters required optional queryString
