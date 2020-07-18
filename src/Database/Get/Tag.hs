{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.Get.Tag where

import           Data.Int                         (Int64)
import           Data.Text                        (Text)
import           Database.PostgreSQL.Simple       (Connection, execute, query)
import           Database.PostgreSQL.Simple       (Only (Only))
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Database.PostgreSQL.Simple.Types (Binary (Binary))
import           Database.PostgreSQL.Simple.Types (Only)
import           Database.Types
import           Api.Types.Tag
import Data.Vector (fromList)

getTags ::
     Connection
  -> Maybe TagId
  -> Maybe [TagId]
  -> Maybe Name
  -> Maybe Limit
  -> Maybe Offset
  -> IO [Tag]
getTags conn mbTagId mbTagsId mbName mbLimit mbOffset= do
  res <-
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
      (mbTagId,fromList <$> mbTagsId, fromList <$> mbTagsId,mbName, mbLimit, mbOffset)
  return $ map tupleToTag res
