{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.CreateCategory where

import           Api.Helpers.Check
import           Api.Helpers.Getters
import qualified Api.Methods.Errors         as Err
import           Api.Types.Response
import           Control.Exception          (SomeException, try)
import           Data.ByteString            (ByteString)
import           Data.ByteString.Char8      (unpack)
import qualified Database.Category          as DB
import           Database.PostgreSQL.Simple (Connection)
import           Database.Types
import qualified Database.User              as DB
import           Network.HTTP.Types         (Status, status200, status400,
                                             status404)

createCategory ::
     Connection -> [(ByteString, Maybe Login)] -> IO (Status, Response ())
createCategory conn queryString = do
  let eitherParameters = checkAndGetParameters required optional queryString
  case eitherParameters of
    Left error -> return (status400, errorResponse error)
    Right (requiredValues, optionalMaybeValues) -> do
      let [token, name] = requiredValues
      let [parentId] = optionalMaybeValues
      maybeUserIdAndPriv <- DB.getMaybeUserIdAndPriv conn (unpack token)
      print maybeUserIdAndPriv
      case maybeUserIdAndPriv of
        [] -> return (status404, badResoponse)
        [(_, False)] -> return (status404, badResoponse)
        [(_, True)] -> do
          res <- try $ DB.addCategory conn name (fromInt <$> parentId)
          case res of
            Left (e :: SomeException) ->
              return (status400, errorResponse Err.smth)
            Right 0 -> return $ (status400, errorResponse Err.noParrent)
            Right 1 -> return $ (status200, okResponse)
  where
    requiredNames = ["token", "name"]
    requiredChecks = [isNotEmpty, isNotEmpty]
    required = (requiredNames, requiredChecks)
    optionalNames = ["parent_id"]
    optionalChecks = [isInt]
    optional = (optionalNames, optionalChecks)
