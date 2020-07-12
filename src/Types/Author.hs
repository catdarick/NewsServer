module Types.Author where

import           Data.Text (Text)

data Author =
  Author
    { authorUserId      :: Integer
    , authorDescription :: Text
    }
