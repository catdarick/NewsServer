{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.PostDraft where

import           Api.Helpers.Check
import           Api.Helpers.Getters

import qualified Api.Methods.Errors         as Err
import           Api.Types.Response
import           Control.Exception          (SomeException)
import           Data.ByteString            (ByteString)
import           Database.PostgreSQL.Simple (Connection)
import           Database.Types
import           Network.HTTP.Types         (status200, Status, status400)
import qualified Database.Author                  as DB
import qualified Database.Draft                    as DB
import Database.PostgreSQL.Simple.Types (Only(Only))
postDraft ::
     Connection -> [(ByteString, Maybe Login)] -> IO (Status, Response ())
postDraft conn queryString = do
  let eitherParameters = checkAndGetParameters required optional queryString
  case eitherParameters of
    Left error -> return (status400, errorResponse error)
    Right (requiredValues, optionalMaybeValues) -> do
      let [token, draftId] = requiredValues
      let [] = optionalMaybeValues
      mbAuthorToken <- DB.getDraftAuthorToken conn (fromInt draftId)
      case mbAuthorToken of
          [] -> return (status400, errorResponse Err.noDraft)
          [Only token] -> DB.publishDraft conn (fromInt draftId) >> return (status200, okResponse)
  where
    requiredNames = ["token", "draft_id"]
    requiredChecks = [isNotEmpty, isInt]
    required = (requiredNames, requiredChecks)
    optionalNames = []
    optionalChecks = []
    optional = (optionalNames, optionalChecks)
