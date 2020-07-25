{-# LANGUAGE OverloadedStrings #-}

module TestHelper where

import           Api.ErrorException
import           Data.Time           (LocalTime)
import           Data.Time.Calendar  (Day (ModifiedJulianDay))
import           Data.Time.LocalTime (LocalTime (LocalTime), midnight)

withEmptyError :: Either ErrorException b -> Either ErrorException b
withEmptyError (Left error) = Left error {excError = ""}
withEmptyError smth         = smth

defTime :: LocalTime
defTime = (LocalTime (ModifiedJulianDay 0) midnight)
