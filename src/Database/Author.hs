{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Database.Author where

import           Data.Int                         (Int64)
import           Data.Text                        (Text)
import           Database.PostgreSQL.Simple       (Connection,
                                                   connectPostgreSQL, execute,
                                                   query)
import           Database.PostgreSQL.Simple       (Only (Only))
import           Database.PostgreSQL.Simple.SqlQQ (sql)
import           Database.PostgreSQL.Simple.Types (Binary (Binary))
import           Database.PostgreSQL.Simple.Types (Only)
import           Database.Types
addAuthor ::
     Connection
  -> UserId
  -> Maybe Description
  -> IO [Only AuthorId]
addAuthor conn userId description  =
  query
    conn
    [sql|
        INSERT INTO author
        (user_id, description)
        VALUES (?,?)|]
    (userId, description)

getAuthorId :: Connection -> Token-> IO [(Only AuthorId)]
getAuthorId conn token =
  query
    conn
    [sql|
        SELECT author.id FROM user_token, user_account, author
        WHERE user_token.token = ?
        AND user_account.id = user_token.user_id
        AND author.user_id = user_account.id
        |]
    (Only token)