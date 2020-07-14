{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.CreateTag where

import           Api.Helpers.Check
import           Api.Helpers.Getters
import qualified Api.Methods.Errors         as Err
import           Api.Types.Response
import           Control.Exception          (SomeException, try)
import           Data.ByteString            (ByteString)
import           Database.PostgreSQL.Simple (Connection)
import qualified Database.Tag               as DB
import           Database.Types
import qualified Database.User              as DB
import           Network.HTTP.Types         (Status, status200, status400,
                                             status404)
import Database.PostgreSQL.Simple.Types (Only(Only))

createTag ::
     Connection -> [(ByteString, Maybe Login)] -> IO (Status, Response Idcont)
createTag conn queryString = do
  let eitherParameters = checkAndGetParameters required optional queryString
  case eitherParameters of
    Left error -> return (status404, errorResponse error)
    Right (requiredValues, optionalMaybeValues) -> do
      let [token, name] = requiredValues
      let [] = optionalMaybeValues
      maybeUserIdAndPriv <- DB.getMaybeUserIdAndPriv conn (token)
      print maybeUserIdAndPriv
      case maybeUserIdAndPriv of
        [] -> return (status404, badResoponse)
        [(_, False)] -> return (status404, badResoponse)
        [(_, True)] -> do
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
