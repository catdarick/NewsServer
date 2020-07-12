{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Migration.User where

import           Database.PostgreSQL.Simple       (execute_)
import           Database.PostgreSQL.Simple.SqlQQ (sql)

createUserTable conn = do
  execute_
    conn
    [sql| CREATE TABLE user_account
        ( id SERIAL PRIMARY KEY
        , username TEXT
        , first_name TEXT
        , second_name TEXT
        , picture TEXT
        , creation_time TIMESTAMP
        , is_admin BOOL
        )|]

createAuthorTable conn = do
  execute_
    conn
    [sql| CREATE TABLE author
        ( user_id UNIQUE INTEGER REFERENCES user_account (id)
        , description TEXT
        )|]

createCategoryTable conn = do
  execute_
    conn
    [sql| CREATE TABLE category
        ( id SERIAL PRIMARY KEY
        , parent INTEGER REFERENCES category (id)
        , title TEXT
        )|]
    

createCommentTable conn = do
  execute_
    conn
    [sql| CREATE TABLE comment
        ( user_id UNIQUE INTEGER REFERENCES user_account (id)
        , username TEXT
        , first_name TEXT
        , second_name TEXT
        , picture TEXT
        , creation_time TIMESTAMP
        , is_admin BOOL
        )|]