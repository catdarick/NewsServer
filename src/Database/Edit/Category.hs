{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Database.Edit.Category where

import           Api.ErrorException
import qualified Api.Errors                       as Err
import           Api.Types.Synonyms
import           Control.Exception                (SomeException, try)
import           Control.Monad.Catch              (MonadThrow (throwM))
import           Control.Monad.Trans.Class        (MonadTrans (lift))
import           Control.Monad.Trans.State        (gets)
import           Data.Int                         (Int64)
import           Database.PostgreSQL.Simple       (Connection, execute)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Network.HTTP.Types               (status400)
import           State.Types

editCategory :: CategoryId -> Maybe Name -> Maybe CategoryId -> ServerStateIO ()
editCategory categoryId mbName mbParentId = do
  conn <- gets conn
  res <- lift $ try $ editCategoryLocal conn categoryId mbName mbParentId
  case res of
    Left (e :: SomeException) -> throwM $ ErrorException status400 Err.noParent
    Right 0 -> throwM $ ErrorException status400 Err.noCategory
    Right 1 -> return ()

editCategoryLocal ::
     Connection -> CategoryId -> Maybe Name -> Maybe CategoryId -> IO Int64
editCategoryLocal conn categoryId mbName (Just 0) =
  execute
    conn
    [sql|
      UPDATE category
      SET
      name = COALESCE(?, name),
      parent_id = null
      WHERE id=?|]
    (mbName, categoryId)
editCategoryLocal conn categoryId mbName mbParentId =
  execute
    conn
    [sql|
      UPDATE category
      SET
      name = COALESCE(?, name),
      parent_id = COALESCE(?, parent_id)
      WHERE id=?|]
    (mbName, mbParentId, categoryId)
