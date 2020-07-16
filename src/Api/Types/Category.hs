{-# LANGUAGE DeriveGeneric #-}

module Api.Types.Category where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Text         (Text)
import           Data.Time         (LocalTime, UTCTime)
import           Database.Types
import           GHC.Generics      (Generic)

data Category =
  Category
    { categoryId     :: Int
    , categoryName   :: Text
    , categoryChilds :: Maybe [Category]
    }
  deriving (Generic, Eq, Show)

instance ToJSON Category where
  toJSON = genericToJSON $ (aesonPrefix snakeCase) {omitNothingFields = True}

getCategory :: Int -> Text -> [Category] -> Category
getCategory id name [] =
  Category {categoryId = id, categoryName = name, categoryChilds = Nothing}
getCategory id name childs =
  Category {categoryId = id, categoryName = name, categoryChilds = Just childs}

