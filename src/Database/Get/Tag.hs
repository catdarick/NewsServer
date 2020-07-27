{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.Get.Tag where

import           Api.Types.Synonyms
import           Api.Types.Tag
import           Control.Monad.Trans.Class        (MonadTrans (lift))
import           Control.Monad.Trans.State        (gets)
import           Data.Vector                      (fromList)
import           Database.PostgreSQL.Simple       (Connection, execute, query)
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           State.Types

getTags ::
     Maybe TagId
  -> Maybe [TagId]
  -> Maybe Name
  -> Maybe Limit
  -> Maybe Offset
  -> ServerStateIO [Tag]
getTags mbTagId mbTagsId mbName mbLimit mbOffset = do
  conn <- gets conn
  res <-
    lift $
    query
      conn
      [sql|
                SELECT id, name
                FROM tag
                WHERE id = COALESCE(?, id)
                AND (? IS NULL OR (SELECT id = ANY (?)))
                AND name = COALESCE(?, name)
                LIMIT COALESCE(?, 50)
                OFFSET COALESCE(?, 0)
                |]
      ( mbTagId
      , fromList <$> mbTagsId
      , fromList <$> mbTagsId
      , mbName
      , mbLimit
      , mbOffset)
  return $ map tupleToTag res
