{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Create.Account where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import           Api.Types.Response
import           Api.Types.Synonyms
import           Config
import           Crypto.Hash.MD5            (hash)
import           Data.ByteString            (ByteString)
import           Data.Function              ((&))
import           Database.Checks.User
import qualified Database.Create.User       as DB
import           Database.PostgreSQL.Simple (Connection)
import qualified Logger.Interact            as Log
import           Network.HTTP.Types         (Status, status201)
import           State.Types

createAccount ::
     [(FieldName, Maybe ByteString)] -> ServerStateIO (Status, Response Idcont)
createAccount queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
  let [login, password, fName, lName] = requiredValues
  let [mbToken, picture] = optionalMaybeValues
  let passHash = hash password
  isAdmin <- isAdmin mbToken
  id <- DB.addUser login passHash fName lName picture isAdmin
  Log.info $ "User with login '" <> login <> "' successfully created"
  return (status201, idResponse id)
  where
    requiredNames = ["login", "password", "first_name", "last_name"]
    requiredChecks =
      [isCorrectLength 5 20, isCorrectLength 6 40, isNotEmpty, isNotEmpty]
    required = (requiredNames, requiredChecks)
    optionalNames = ["token", "picture"]
    optionalChecks = [isNotEmpty, isNotEmpty]
    optional = (optionalNames, optionalChecks)
    parameters = checkAndGetParameters required optional queryString
    isAdmin = maybe (return False) isAdminToken
