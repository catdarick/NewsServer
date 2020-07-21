{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Create.Category where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import qualified Api.Methods.Errors               as Err
import           Api.Types
import           Api.Types.Response
import           Control.Exception                (SomeException, try)
import           Data.ByteString                  (ByteString)
import qualified Database.Checks.User             as DB
import qualified Database.Create.Category         as DB
import           Database.PostgreSQL.Simple       (Connection)
import           Database.PostgreSQL.Simple.Types (Only (Only))
import           Network.HTTP.Types               (Status, status200, status400,
                                                   status404)

createCategory ::
     Connection -> [(ByteString, Maybe Login)] -> IO (Status, Response Idcont)
createCategory conn queryString = do
  let eitherParameters = checkAndGetParameters required optional queryString
  case eitherParameters of
    Left error -> return (status404, errorResponse error)
    Right (requiredValues, optionalMaybeValues) -> do
      let [token, name] = requiredValues
      let [parentId] = optionalMaybeValues
      isAdmin <- DB.isAdminToken conn token
      if not isAdmin
        then return (status404, badResoponse)
        else do
          res <- try $ DB.addCategory conn name (toInt <$> parentId)
          case res of
            Left (e :: SomeException) ->
              return (status400, errorResponse Err.smth)
            Right [] -> return $ (status400, errorResponse Err.noParrent)
            Right [Only id] -> return $ (status200, idResponse id)
  where
    requiredNames = ["token", "name"]
    requiredChecks = [isNotEmpty, isNotEmpty]
    required = (requiredNames, requiredChecks)
    optionalNames = ["parent_id"]
    optionalChecks = [isInt]
    optional = (optionalNames, optionalChecks)
