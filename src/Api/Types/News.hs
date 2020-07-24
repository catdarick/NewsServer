{-# LANGUAGE DeriveGeneric #-}

module Api.Types.News where

import           Api.Types.Author
import           Api.Types.Category
import           Api.Types.Comment
import           Api.Types.Tag
import           Api.Types.User
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Text                  (Text)
import           Data.Time                  (LocalTime, UTCTime)
import           Data.Vector                (Vector)
import           Database.Get.Tag
import           Database.PostgreSQL.Simple (Connection)
import           Api.Types.Synonyms
import           GHC.Generics               (Generic)

data News =
  News
    { newsId                 :: Int
    , newsTitle              :: Text
    , newsCreationTime       :: LocalTime
    , newsAuthor             :: Maybe Author
    , newsTags               :: [Tag]
    , newsContent            :: Text
    , newsMainPicture        :: Maybe PictureText
    , newsAdditionalPictures :: Maybe (Vector PictureText)
    , newsCategory           :: Category
    }
  deriving (Generic, Eq, Show)

instance ToJSON News where
  toJSON = genericToJSON $ (aesonPrefix snakeCase) {omitNothingFields = True}

tupleToNews (id, title, time, content, mainPic, addPics, tags, category, authorId, mbDescr, userId, login, fName, lName, userPic, userCrTime, userIsAdmin) =
  News
    { newsId = id
    , newsTitle = title
    , newsCreationTime = time
    , newsContent = content
    , newsMainPicture = mainPic
    , newsAdditionalPictures = addPics
    , newsAuthor = Just author
    , newsTags = tags
    , newsCategory = category
    }
  where
    author =
      tupleToAuthor
        ( authorId
        , mbDescr
        , userId
        , login
        , fName
        , lName
        , userPic
        , userCrTime
        , userIsAdmin)

tupleToDraft (id, title, time, content, mainPic, addPics, tags, category) =
  News
    { newsId = id
    , newsTitle = title
    , newsCreationTime = time
    , newsContent = content
    , newsMainPicture = mainPic
    , newsAdditionalPictures = addPics
    , newsAuthor = Nothing
    , newsTags = tags
    , newsCategory = category
    }
