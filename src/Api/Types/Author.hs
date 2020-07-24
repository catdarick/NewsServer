{-# LANGUAGE DeriveGeneric #-}

module Api.Types.Author where

import           Api.Types.User
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Text         (Text)
import           Data.Time         (LocalTime, UTCTime)
import           Api.Types.Synonyms
import           GHC.Generics      (Generic)

data Author =
  Author
    { authorId          :: Int
    , authorUser        :: User
    , authorDescription :: Maybe Text
    }
  deriving (Generic, Eq, Show)

instance ToJSON Author where
  toJSON = genericToJSON $ (aesonPrefix snakeCase) {omitNothingFields = True}

tupleToAuthor ::
     ( Int
     , Maybe Text
     , Int
     , Text
     , Text
     , Text
     , Maybe PictureText
     , LocalTime
     , Bool)
  -> Author
tupleToAuthor (id, mbDescr, userId, login, fName, lName, userPic, userCrTime, userIsAdmin) =
  Author {authorId = id, authorUser = user, authorDescription = mbDescr}
  where
    user =
      tupleToUser
        (userId, login, fName, lName, userPic, userCrTime, userIsAdmin)
