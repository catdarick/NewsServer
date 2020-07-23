{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Edit.Draft where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import qualified Api.Methods.Errors               as Err
import           Api.Types
import           Api.Types.Response
import           Data.ByteString                  (ByteString)
import qualified Database.Checks.Draft            as DB
import qualified Database.Checks.User             as DB
import qualified Database.Edit.Draft              as DB
import qualified Database.Get.Draft               as DB
import           Database.PostgreSQL.Simple       (Connection)
import           Database.PostgreSQL.Simple.Types (Only (Only))
import           Network.HTTP.Types               (Status, status200, status400,
                                                   status403, status404)

editDraft :: Connection -> [(ByteString, Maybe Login)] -> IO (Response ())
editDraft conn queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
  let [token, draftId] = requiredValues
  let [title, content, categoryId, tagsId, mainPicture, pictures] =
        optionalMaybeValues
  DB.draftAuthorGuard conn (toInt draftId) token
  DB.editDraft
    conn
    (toInt draftId)
    title
    content
    (toInt <$> categoryId)
    mainPicture
    (toStringList <$> pictures)
    (toIntList <$> tagsId)
  return okResponse
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
