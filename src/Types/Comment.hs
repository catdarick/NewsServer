module Types.Comment where

import           Data.Text    (Text)
import           Data.Time    (UTCTime)
import           Types.Author

data Comment =
  Comment
    { commentId           :: Integer
    , commentAuthor       :: Author
    , commentCreationTime :: UTCTime
    , commentContent      :: Text
    }
