{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.EditDraft where

import           Api.Helpers.Check
import           Api.Helpers.Getters
import qualified Api.Methods.Errors               as Err
import           Api.Types.Response
import           Control.Exception                (SomeException, try)
import           Data.ByteString                  (ByteString)
import           Data.ByteString.Char8            (unpack)
import qualified Database.Draft                   as DB
import           Database.PostgreSQL.Simple       (Connection)
import           Database.PostgreSQL.Simple.Types (Only (Only))
import           Database.Types
import qualified Database.User                    as DB
import           Network.HTTP.Types               (Status, status200, status400,
                                                   status403, status404)

editDraft ::
     Connection -> [(ByteString, Maybe Login)] -> IO (Status, Response Idcont)
editDraft conn queryString = do
  let eitherParameters = checkAndGetParameters required optional queryString
  case eitherParameters of
    Left error -> return (status400, errorResponse error)
    Right (requiredValues, optionalMaybeValues) -> do
      let [token, draftId] = requiredValues
      let [title, content, categoryId, tagsId, mainPicture, pictures] =
            optionalMaybeValues
      let performEditAndReturnOk = do
            DB.editDraft
              conn
              (fromInt draftId)
              title
              content
              (fromInt <$> categoryId)
              mainPicture
              (fromStringList <$> pictures)
            return (status400, okResponse)
      maybeUserIdAndPriv <- DB.getMaybeUserIdAndPriv conn token
      mbAuthorToken <- DB.getDraftAuthorToken conn (fromInt draftId)
      case mbAuthorToken of
        [] -> return (status400, errorResponse Err.noDraft)
        [Only authorToken] ->
          if authorToken == token
            then performEditAndReturnOk
            else case maybeUserIdAndPriv of
                   [(_, True)] -> performEditAndReturnOk
                   _ -> return (status403, errorResponse Err.noPerms)
  where
    requiredNames = ["token", "draft_id"]
    requiredChecks = [isNotEmpty, isInt]
    required = (requiredNames, requiredChecks)
    optionalNames =
      ["title", "content", "category_id", "tags_id", "main_picture", "pictures"]
    optionalChecks =
      [isNotEmpty, isNotEmpty, isInt, isIntList, isNotEmpty, isNotEmptyTextList]
    optional = (optionalNames, optionalChecks)