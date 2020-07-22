{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Create.Account where

import           Api.ErrorException
import           Api.Helpers.Checks
import           Api.Helpers.Getters
import qualified Api.Methods.Errors               as Err
import           Api.Types
import           Api.Types.Response
import           Config
import           Control.Exception                (SomeException, try)
import           Control.Monad.Catch              (MonadThrow (throwM))
import           Crypto.Hash.MD5                  (hash)
import           Data.ByteString                  (ByteString)
import           Data.Function                    ((&))
import           Data.Maybe                       (isJust, isNothing)
import qualified Database.Create.User             as DB
import           Database.PostgreSQL.Simple       (Connection)
import           Database.PostgreSQL.Simple.Types (Only (Only))
import           Network.HTTP.Types.Status

createAccount ::
     Connection
  -> Config
  -> [(FieldName, Maybe ByteString)]
  -> IO (Response Idcont)
createAccount conn config queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
  let [login, password, fName, lName] = requiredValues
  let [adminPass, picture] = optionalMaybeValues
  let passHash = hash password
  id <- DB.addUser conn login passHash fName lName picture (isJust adminPass)
  return $ idResponse id
  where
    requiredNames = ["login", "password", "first_name", "last_name"]
    requiredChecks =
      [isCorrectLength 5 20, isCorrectLength 6 40, isNotEmpty, isNotEmpty]
    required = (requiredNames, requiredChecks)
    optionalNames = ["admin_pass", "picture"]
    optionalChecks = [isGlobalAdminPass (config & globalAdminPass), isNotEmpty]
    optional = (optionalNames, optionalChecks)
    parameters = checkAndGetParameters required optional queryString
