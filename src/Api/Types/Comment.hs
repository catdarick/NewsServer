{-# LANGUAGE DeriveGeneric #-}

module Api.Types.Comment where

import           Api.Types.User
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Text         (Text)
import           Data.Time         (LocalTime, UTCTime)
import           Database.Types
import           GHC.Generics      (Generic)

data Comment =
  Comment
    { commentId           :: Integer
    , commentUser         :: User
    , commentCreationTime :: LocalTime
    , commentContent      :: Text
    }
  deriving (Generic, Eq, Show)

instance ToJSON Comment where
  toJSON = genericToJSON $ (aesonPrefix snakeCase) {omitNothingFields = True}

comment id user time content =
  Comment
    { commentId = id
    , commentUser = user
    , commentCreationTime = time
    , commentContent = content
    }
tupleToComment (id, content, time, userId, login, fName, lName, picture, creationTime, isAdmin) = 
  comment id user time content
  where 
    user = tupleToUser (userId, login, fName, lName, picture, creationTime, isAdmin)