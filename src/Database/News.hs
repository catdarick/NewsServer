{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.News where

import           Data.Int                         (Int64)
import           Data.Text                        (Text)
import           Database.PostgreSQL.Simple       (Connection, execute, query)
import           Database.PostgreSQL.Simple       (Only (Only))
import           Database.PostgreSQL.Simple       (In (In))
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Database.PostgreSQL.Simple.Types (Binary (Binary))
import           Database.PostgreSQL.Simple.Types (Only)
import           Database.Types
import Data.Vector (fromList)

addNews ::
     Connection
  -> AuthorId
  -> Title
  -> Content
  -> CategoryId
  -> Maybe Picture
  -> Maybe [Picture]
  -> IO Int64
addNews conn authorId title content caregoryId picture pictures =
  execute
    conn
    [sql|
        INSERT INTO news
        (author_id, title, content, category_id, main_picture, pictures)
        VALUES (?,?,?,?,?,?)|]
    (authorId, title, content, caregoryId, picture, fromList <$> pictures)
