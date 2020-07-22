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
     Connection -> [(ByteString, Maybe Login)] -> IO (Response Idcont)
createCategory conn queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
  let [token, name] = requiredValues
  let [parentId] = optionalMaybeValues
  DB.adminGuard conn token
  id <- DB.addCategory conn name (toInt <$> parentId)
  return $ idResponse id
  where
    requiredNames = ["token", "name"]
    requiredChecks = [isNotEmpty, isNotEmpty]
    required = (requiredNames, requiredChecks)
    optionalNames = ["parent_id"]
    optionalChecks = [isInt]
    optional = (optionalNames, optionalChecks)
    parameters = checkAndGetParameters404 required optional queryString
