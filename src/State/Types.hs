module State.Types where

import           Config
import           Database.PostgreSQL.Simple (Connection)

data State =
  State
    { dbConn :: Connection
    , config :: Config
    }
state conn config = State {dbConn = conn, config=config}