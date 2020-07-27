{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Delete.News where

import           Api.ErrorException
import qualified Api.Errors                       as Err
import           Api.Types.Synonyms
import           Control.Monad.Catch              (MonadThrow (throwM))
import           Control.Monad.Trans.Class        (MonadTrans (lift))
import           Control.Monad.Trans.State        (gets)
import           Data.Int                         (Int64)
import           Database.PostgreSQL.Simple       (Connection, Only (Only),
                                                   execute)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Network.HTTP.Types.Status        (status400)
import           State.Types

deleteNews :: NewsId -> ServerStateIO ()
deleteNews newsId = do
  conn <- gets conn
  res <-
    lift $
    execute
      conn
      [sql|
    DELETE FROM news
    WHERE id=?
    AND is_published = true|]
      (Only newsId)
  case res of
    0 -> throwM $ ErrorException status400 Err.noNews
    1 -> return ()
