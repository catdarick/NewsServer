{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Edit.Draft where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import           Api.Types.Response
import           Api.Types.Synonyms
import           Data.ByteString            (ByteString)
import qualified Database.Checks.Draft      as DB
import qualified Database.Checks.User       as DB
import qualified Database.Edit.Draft        as DB
import qualified Database.Get.Draft         as DB
import           Database.PostgreSQL.Simple (Connection)
import qualified Logger.Interact            as Log
import           Network.HTTP.Types         (Status, status200)
import           State.Types

editDraft :: [(ByteString, Maybe Login)] -> ServerStateIO (Status, Response ())
editDraft queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
  let [token, draftId] = requiredValues
  let [title, content, categoryId, tagsId, mainPicture, pictures] =
        optionalMaybeValues
  DB.draftAuthorGuard (toInt draftId) token
  DB.editDraft
    (toInt draftId)
    title
    content
    (toInt <$> categoryId)
    mainPicture
    (toStringList <$> pictures)
    (toIntList <$> tagsId)
  Log.info $ "Draft '" <> draftId <> "' successfully edited"
  return (status200, okResponse)
  where
    requiredNames = ["token", "draft_id"]
    requiredChecks = [isNotEmpty, isInt]
    required = (requiredNames, requiredChecks)
    optionalNames =
      ["title", "content", "category_id", "tags_id", "main_picture", "pictures"]
    optionalChecks =
      [isNotEmpty, isNotEmpty, isInt, isIntList, isNotEmpty, isNotEmptyTextList]
    optional = (optionalNames, optionalChecks)
    parameters = checkAndGetParameters required optional queryString
