module Types.News where

import           Data.Text     (Text)
import           Data.Time     (UTCTime)
import           Types.Author
import           Types.Comment
import           Types.Picture
import           Types.Tag

data News =
  News
    { newsId                 :: Integer
    , newsTitle              :: Text
    , newsCreationTime       :: UTCTime
    , newsAuthor             :: Author
    , newsTags               :: [Tag]
    , newsContent            :: Text
    , newsMainPicture        :: Picture
    , newsAdditionalPictures :: [Picture]
    , newsComments           :: [Comment]
    }
