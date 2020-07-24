{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Delete.Comment where

import           Api.ErrorException
import qualified Api.Errors                       as Err
import           Api.Types.Synonyms
import           Control.Monad.Catch              (MonadThrow (throwM))
import           Data.Int                         (Int64)
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   execute)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Network.HTTP.Types.Status        (status400)

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
