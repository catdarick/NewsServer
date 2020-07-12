module Types.Category where

import           Data.Text (Text)

data Category =
  Category
    { categoryId     :: Integer
    , categoryTitle  :: String
    , categoryChilds :: [Category]
    }
