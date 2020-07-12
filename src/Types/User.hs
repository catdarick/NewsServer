module Types.User where

import           Data.Text     (Text)
import           Data.Time     (UTCTime)
import           Types.Picture

data User =
  User
    { userId           :: Integer
    , userFirstName    :: Text
    , userSecondName   :: Text
    , userPicture      :: Picture
    , userCreationTime :: UTCTime
    , userIsAdmin      :: Bool
    }
