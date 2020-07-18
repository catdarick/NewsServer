{-# LANGUAGE DeriveGeneric #-}

module Api.Types.Tag where

import           Data.Text         (Text)

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Text         (Text)
import           Data.Time         (LocalTime, UTCTime)
import           Api.Types
import           GHC.Generics      (Generic)

data Tag =
  Tag
    { tagId   :: Integer
    , tagName :: Text
    }
  deriving (Generic, Eq, Show)

instance ToJSON Tag where
  toJSON = genericToJSON $ (aesonPrefix snakeCase) {omitNothingFields = True}

tupleToTag :: (Integer, Text) -> Tag
tupleToTag (id, name) = Tag {tagId = id, tagName = name}
