{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Delete.Tag where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import qualified Api.Methods.Errors               as Err
import           Api.Types
import           Api.Types.Response
import           Control.Exception                (SomeException, try)
import           Data.ByteString                  (ByteString)
import qualified Database.Checks.User             as DB
import qualified Database.Delete.Tag              as DB
import           Database.PostgreSQL.Simple       (Connection)
import           Database.PostgreSQL.Simple.Types (Only (Only))
import           Network.HTTP.Types               (Status, status200, status400,
                                                   status404)

deleteTag ::
     Connection -> [(ByteString, Maybe Login)] -> IO (Status, Response Idcont)
deleteTag conn queryString = do
  let eitherParameters = checkAndGetParametersEither required optional queryString
  case eitherParameters of
    Left error -> return (status404, badResponse)
    Right (requiredValues, optionalMaybeValues) -> do
      let [token, tagId] = requiredValues
      let [] = optionalMaybeValues
      isAdmin <- DB.isAdminToken conn token
      if not isAdmin
        then return (status404, badResponse)
        else do
          res <- try $ DB.deleteTag conn (toInt tagId)
          case res of
            Left (e :: SomeException) ->
              return (status400, errorResponse Err.smth)
            Right 0 -> return $ (status400, errorResponse Err.noTag)
            Right 1 -> return $ (status200, okResponse)
  where
    requiredNames = ["token", "tag_id"]
    requiredChecks = [isNotEmpty, isInt]
    required = (requiredNames, requiredChecks)
    optionalNames = []
    optionalChecks = []
    optional = (optionalNames, optionalChecks)
