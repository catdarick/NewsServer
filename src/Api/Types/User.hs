{-# LANGUAGE DeriveGeneric #-}

module Api.Types.User where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Text         (Text)
import           Data.Time         (LocalTime, UTCTime)
import           Database.Types
import           GHC.Generics      (Generic)

data User =
  User
    { userId           :: Integer
    , userLogin        :: Text
    , userFirstName    :: Text
    , userLastName     :: Text
    , userPicture      :: Maybe PictureText
    , userCreationTime :: LocalTime
    , userIsAdmin      :: Bool
    }
  deriving (Generic, Eq, Show)

instance ToJSON User where
  toJSON = genericToJSON $ (aesonPrefix snakeCase) {omitNothingFields = True}

tupleToUser ::
     (Integer, Text, Text, Text, Maybe PictureText, LocalTime, Bool) -> User
tupleToUser (id, login, fName, lName, picture, creationTime, isAdmin) =
  User
    { userId = id
    , userLogin = login
    , userFirstName = fName
    , userLastName = lName
    , userPicture = picture
    , userCreationTime = creationTime
    , userIsAdmin = isAdmin
    }
