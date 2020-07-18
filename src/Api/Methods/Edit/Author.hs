{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Edit.Author where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import qualified Api.Methods.Errors               as Err
import           Api.Types
import           Api.Types.Response
import           Control.Exception                (SomeException, try)
import           Data.ByteString                  (ByteString)
import qualified Database.Checks.User             as DB
import qualified Database.Edit.Author             as DB
import           Database.PostgreSQL.Simple       (Connection)
import           Database.PostgreSQL.Simple.Types (Only (Only))
import           Network.HTTP.Types               (Status, status200, status400,
                                                   status404)

editAuthor ::
     Connection -> [(ByteString, Maybe Login)] -> IO (Status, Response Idcont)
editAuthor conn queryString = do
  let eitherParameters = checkAndGetParameters required optional queryString
  case eitherParameters of
    Left error -> return (status404, badResoponse)
    Right (requiredValues, optionalMaybeValues) -> do
      let [token, authorId] = requiredValues
      let [description] = optionalMaybeValues
      isAdmin <- DB.isAdminToken conn token
      if not isAdmin
        then return (status404, badResoponse)
        else do
          res <- try $ DB.editAuthor conn (toInt authorId) description
          case res of
            Left (e :: SomeException) ->
              return (status400, errorResponse Err.smth)
            Right 0 -> return (status400, errorResponse Err.noAuthor)
            Right 1 -> return (status200, okResponse)
  where
    requiredNames = ["token", "author_id"]
    requiredChecks = [isNotEmpty, isInt]
    required = (requiredNames, requiredChecks)
    optionalNames = ["description"]
    optionalChecks = [isNotEmpty]
    optional = (optionalNames, optionalChecks)
