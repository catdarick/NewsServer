{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Create.Tag where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import qualified Api.Methods.Errors               as Err
import           Api.Types
import           Api.Types.Response
import           Control.Exception                (SomeException, try)
import           Data.ByteString                  (ByteString)
import qualified Database.Checks.User             as DB
import qualified Database.Create.Tag              as DB
import           Database.PostgreSQL.Simple       (Connection)
import           Database.PostgreSQL.Simple.Types (Only (Only))
import           Network.HTTP.Types               (Status, status200, status400,
                                                   status404)

createTag ::
     Connection -> [(ByteString, Maybe Login)] -> IO (Status, Response Idcont)
createTag conn queryString = do
  let eitherParameters = checkAndGetParameters required optional queryString
  case eitherParameters of
    Left error -> return (status404, errorResponse error)
    Right (requiredValues, optionalMaybeValues) -> do
      let [token, name] = requiredValues
      let [] = optionalMaybeValues
      isAdmin <- DB.isAdminToken conn token
      if not isAdmin
        then return (status404, badResoponse)
        else do
          res <- try $ DB.addTag conn name
          case res of
            Left (e :: SomeException) ->
              return (status400, errorResponse Err.tagExists)
            Right [] -> return $ (status400, errorResponse Err.smth)
            Right [Only id] -> return $ (status200, idResponse id)
  where
    requiredNames = ["token", "name"]
    requiredChecks = [isNotEmpty, isNotEmpty]
    required = (requiredNames, requiredChecks)
    optionalNames = []
    optionalChecks = []
    optional = (optionalNames, optionalChecks)
