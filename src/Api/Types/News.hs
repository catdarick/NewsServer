{-# LANGUAGE DeriveGeneric #-}

module Api.Types.News where

import           Api.Types.Author
import           Api.Types.Comment
import           Api.Types.Tag
import           Api.Types.User
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Text         (Text)
import           Data.Time         (LocalTime, UTCTime)
import           Data.Vector       (Vector)
import           Database.Types
import           GHC.Generics      (Generic)

data News =
  News
    { newsId                 :: Integer
    , newsTitle              :: Text
    , newsCreationTime       :: LocalTime
    , newsAuthor             :: Author
    , newsTags               :: [Tag]
    , newsContent            :: Text
    , newsMainPicture        :: Maybe PictureText
    , newsAdditionalPictures :: Maybe (Vector PictureText)
    , newsComments           :: [Comment]
    }
  deriving (Generic, Eq, Show)

instance ToJSON News where
  toJSON = genericToJSON $ (aesonPrefix snakeCase) {omitNothingFields = True}

tupleToNews (id, title, time, content, mainPic, addPics, authorId, mbDescr, userId, login, fName, lName, userPic, userCrTime, userIsAdmin) =
  News
    { newsId = id
    , newsTitle = title
    , newsCreationTime = time
    , newsContent = content
    , newsMainPicture = mainPic
    , newsAdditionalPictures = addPics
    , newsAuthor = author
    , newsTags = []
    , newsComments = []
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
