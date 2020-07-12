module Types.Tag where

import           Data.Text (Text)

data Tag =
  Tag
    { tagId    :: Integer
    , tagTitle :: Text
    }
