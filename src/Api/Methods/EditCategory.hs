{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.EditCategory where

import           Api.Helpers.Check
import           Api.Helpers.Getters
import qualified Api.Methods.Errors         as Err
import           Api.Types.Response
import           Control.Exception          (SomeException, try)
import           Data.ByteString            (ByteString)
import           Data.ByteString.Char8      (unpack)
import qualified Database.Category            as DB
import           Database.PostgreSQL.Simple (Connection)
import           Database.Types
import qualified Database.User              as DB
import           Network.HTTP.Types         (Status, status200, status400,
                                             status404)
import Database.PostgreSQL.Simple.Types (Only(Only))

editCategory ::
     Connection -> [(ByteString, Maybe Login)] -> IO (Status, Response Idcont)
editCategory conn queryString = do
  let eitherParameters = checkAndGetParameters required optional queryString
  case eitherParameters of
    Left error -> return (status404, badResoponse)
    Right (requiredValues, optionalMaybeValues) -> do
      let [token, categoryId] = requiredValues
      let [name, parentId] = optionalMaybeValues
      maybeUserIdAndPriv <- DB.getMaybeUserIdAndPriv conn token
      case maybeUserIdAndPriv of
        [] -> return (status404, badResoponse)
        [(_, False)] -> return (status404, badResoponse)
        [(_, True)] -> do
          res <- try $ DB.editCategory conn (fromInt categoryId) name (fromInt <$> parentId)
          case res of
            Left (e :: SomeException) ->
              return (status400, errorResponse Err.noParrent)
            Right 0 -> return $ (status400, errorResponse Err.smth)
            Right 1 -> return $ (status200, okResponse)
  where
    requiredNames = ["token", "category_id"]
    requiredChecks = [isNotEmpty, isInt]
    required = (requiredNames, requiredChecks)
    optionalNames = ["name", "parent_id"]
    optionalChecks = [isNotEmpty, isInt]
    optional = (optionalNames, optionalChecks)
