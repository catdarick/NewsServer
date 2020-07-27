{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Delete.Category where

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
import           Network.HTTP.Types               (status400)
import           State.Types

deleteCategory :: CategoryId -> ServerStateIO ()
deleteCategory categoryId = do
  conn <- gets conn
  res <-
    lift $
    execute
      conn
      [sql|
       DELETE FROM category
       WHERE id=?
      |]
      (Only categoryId)
  case res of
    0 -> throwM $ ErrorException status400 Err.noCategory
    1 -> return ()
