{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Get.Category where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import qualified Api.Methods.Errors               as Err
import           Api.Types
import           Api.Types.Category
import           Api.Types.Response
import           Data.ByteString                  (ByteString)
import           Data.Maybe                       (isJust)
import qualified Database.Get.Category            as DB
import           Database.PostgreSQL.Simple       (Connection)
import           Database.PostgreSQL.Simple.Types (Only (Only))
import           Network.HTTP.Types.Status

getCategories ::
     Connection
  -> [(ByteString, Maybe ByteString)]
  -> IO (Status, Response [Category])
getCategories conn queryString = do
  let eitherParameters = checkAndGetParametersEither required optional queryString
  case eitherParameters of
    Left error -> return (status400, errorResponse error)
    Right (requiredValues, optionalMaybeValues) -> do
      let [] = requiredValues
      let [categoryId, parentId, name] = optionalMaybeValues
      categories <-
        if isJust categoryId || isJust parentId || isJust name
          then DB.getCategoriesTreeFromTop
                 conn
                 (toInt <$> categoryId)
                 (toInt <$> parentId)
                 name
          else DB.getRootCategoriesTree conn
      return (status200, payloadResponse categories)
  where
    requiredNames = []
    requiredChecks = []
    required = (requiredNames, requiredChecks)
    optionalNames = ["category_id", "parent_id", "name"]
    optionalChecks = [isInt, isInt, isNotEmpty]
    optional = (optionalNames, optionalChecks)
