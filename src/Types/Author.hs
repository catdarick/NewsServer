module Types.Author where

import           Data.Text      (Text)
import           Database.Types

data Author =
  Author
    { authorId          :: Int
    , authorUserId      :: Int
    , authorDescription :: Maybe Text
    }
