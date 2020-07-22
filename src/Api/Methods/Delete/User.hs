{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Delete.User where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import qualified Api.Methods.Errors               as Err
import           Api.Types
import           Api.Types.Response
import           Control.Exception                (SomeException, try)
import           Data.ByteString                  (ByteString)
import qualified Database.Checks.User             as DB
import qualified Database.Delete.User             as DB
import           Database.PostgreSQL.Simple       (Connection)
import           Database.PostgreSQL.Simple.Types (Only (Only))
import           Network.HTTP.Types               (Status, status200, status400,
                                                   status404)

deleteUser ::
     Connection -> [(ByteString, Maybe Login)] -> IO (Response ())
deleteUser conn queryString = do
   (requiredValues, optionalMaybeValues) <- parameters
   let [token, userId] = requiredValues
   let [] = optionalMaybeValues
   DB.adminGuard conn token
   DB.deleteUser conn (toInt userId)
   return okResponse
  where
    requiredNames = ["token", "user_id"]
    requiredChecks = [isNotEmpty, isInt]
    required = (requiredNames, requiredChecks)
    optionalNames = []
    optionalChecks = []
    optional = (optionalNames, optionalChecks)
    parameters = checkAndGetParameters404 required optional queryString
