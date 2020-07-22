module Api.ErrorException where

import           Api.Types.Response
import           Control.Exception         (Exception)
import           Data.ByteString           (ByteString)
import           Network.HTTP.Types.Status (Status)

data ErrorException =
  ErrorException
    { status   :: Status
    , excError :: ByteString
    }
  deriving (Eq,Show)

instance Exception ErrorException

fromException :: ErrorException -> (Status, ByteString)
fromException exc = (status exc, excError exc)

toException :: (Status, ByteString) -> ErrorException
toException (status, err) = ErrorException {status=status, excError = err}
