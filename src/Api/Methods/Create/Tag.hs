{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Create.Tag where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import           Api.Types.Response
import           Api.Types.Synonyms
import           Data.ByteString            (ByteString)
import qualified Database.Checks.User       as DB
import qualified Database.Create.Tag        as DB
import           Database.PostgreSQL.Simple (Connection)

createTag :: Connection -> [(ByteString, Maybe Login)] -> IO (Response Idcont)
createTag conn queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
  let [token, name] = requiredValues
  let [] = optionalMaybeValues
  DB.adminGuard conn token
  id <- DB.addTag conn name
  return $ idResponse id
  where
    requiredNames = ["token", "name"]
    requiredChecks = [isNotEmpty, isNotEmpty]
    required = (requiredNames, requiredChecks)
    optionalNames = []
    optionalChecks = []
    optional = (optionalNames, optionalChecks)
    parameters = checkAndGetParameters404 required optional queryString
