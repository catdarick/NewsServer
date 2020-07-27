module State.Types where

import           Config
import           Control.Monad.Trans.State  (StateT)
import           Database.PostgreSQL.Simple (Connection)
import           Network.Socket

type ServerStateIO = StateT ServerState IO

data ServerState =
  ServerState
    { config     :: Config
    , conn       :: Connection
    , clientInfo :: SockAddr
    }
