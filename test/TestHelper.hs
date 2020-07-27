{-# LANGUAGE OverloadedStrings #-}

module TestHelper where

import           Api.ErrorException
import           Config
import           Control.Monad.Trans.State  (StateT, evalStateT)
import           Data.Time                  (LocalTime)
import           Data.Time.Calendar         (Day (ModifiedJulianDay))
import           Data.Time.LocalTime        (LocalTime (LocalTime), midnight)
import           Database.PostgreSQL.Simple (Connection)
import           Logger.Types
import           State.Types
import Network.Socket.Internal (SockAddr(SockAddrUnix))

withEmptyError :: Either ErrorException b -> Either ErrorException b
withEmptyError (Left error) = Left error {excError = ""}
withEmptyError smth         = smth

defTime :: LocalTime
defTime = (LocalTime (ModifiedJulianDay 0) midnight)

cfg :: Config
cfg = Config "" "" "" "" "" "" DEBUG

runWithState :: Monad m => Connection -> StateT ServerState m a -> m a
runWithState conn f = evalStateT f (ServerState cfg conn (SockAddrUnix "127.0.0.1:3000"))
