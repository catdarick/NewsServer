{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Methods.Get.Category where

import           Api.Helpers.Checks
import           Api.Helpers.Getters
import           Api.Types.Category
import           Api.Types.Response
import           Api.Types.Synonyms
import           Data.ByteString            (ByteString)
import           Data.Maybe                 (isJust)
import qualified Database.Get.Category      as DB
import           Database.PostgreSQL.Simple (Connection)
import           Network.HTTP.Types         (Status, status200)
import           State.Types
import qualified Logger.Interact            as Log

getCategories ::
     [(ByteString, Maybe ByteString)]
  -> ServerStateIO (Status, Response [Category])
getCategories queryString = do
  (requiredValues, optionalMaybeValues) <- parameters
  let [] = requiredValues
  let [categoryId, parentId, name] = optionalMaybeValues
  categories <-
    if isJust categoryId || isJust parentId || isJust name
      then DB.getCategoriesTreeFromTop
             (toInt <$> categoryId)
             (toInt <$> parentId)
             name
      else DB.getRootCategoriesTree
  return (status200, payloadResponse categories)
  where
    requiredNames = []
    requiredChecks = []
    required = (requiredNames, requiredChecks)
    optionalNames = ["category_id", "parent_id", "name"]
    optionalChecks = [isInt, isInt, isNotEmpty]
    optional = (optionalNames, optionalChecks)
    parameters = checkAndGetParameters required optional queryString
