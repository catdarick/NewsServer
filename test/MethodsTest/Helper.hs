{-# LANGUAGE OverloadedStrings #-}
module MethodsTest.Helper where

import           Api.ErrorException

withEmptyError :: Either ErrorException b -> Either ErrorException b
withEmptyError (Left error) = Left error {excError = ""}
withEmptyError smth         = smth