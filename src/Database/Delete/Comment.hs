{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Delete.Comment where

import           Data.Int                         (Int64)
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   execute)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Api.Types
import           Api.ErrorException
import qualified Api.Methods.Errors             as Err
import Network.HTTP.Types.Status (status400)
import Control.Monad.Catch (MonadThrow(throwM))

deleteComment :: Connection -> CommentId -> IO ()
deleteComment conn commentId = do
  res <-
    execute
      conn
      [sql|
    DELETE FROM comment
    WHERE id=?|]
      (Only commentId)
  case res of
    0 -> throwM $ ErrorException status400 Err.noComment
    1 -> return ()