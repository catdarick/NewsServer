{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Migration.Create where

import           Database.PostgreSQL.Simple       (execute_)
import           Database.PostgreSQL.Simple.SqlQQ (sql)

createUserTable conn = do
  execute_
    conn
    [sql| CREATE TABLE user_account
        ( id SERIAL UNIQUE PRIMARY KEY
        , login TEXT UNIQUE
        , password_hash BYTEA
        , first_name TEXT
        , last_name TEXT
        , picture TEXT
        , creation_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        , is_admin BOOL
        )|]

createAuthorTable conn = do
  execute_
    conn
    [sql| CREATE TABLE author 
        ( id SERIAL UNIQUE PRIMARY KEY
        , user_id INTEGER UNIQUE REFERENCES user_account (id) ON DELETE CASCADE
        , description TEXT
        )|]

createUserTokenTable conn = do
  execute_
    conn
    [sql| CREATE TABLE user_token
        ( user_id INTEGER UNIQUE REFERENCES user_account (id) ON DELETE CASCADE
        , token TEXT
        )|]

createCategoryTable conn = do
  execute_
    conn
    [sql| CREATE TABLE category
        ( id SERIAL UNIQUE PRIMARY KEY
        , parent_id INTEGER REFERENCES category (id) ON DELETE RESTRICT
        , name TEXT
        )|]

createTagTable conn = do
  execute_
    conn
    [sql| CREATE TABLE tag
        ( id SERIAL UNIQUE PRIMARY KEY
        , name TEXT UNIQUE
        )|]

createNewsTable conn = do
  execute_
    conn
    [sql| CREATE TABLE news
        ( id SERIAL UNIQUE PRIMARY KEY
        , author_id INTEGER REFERENCES author (id) ON DELETE SET NULL
        , category_id INTEGER REFERENCES category (id) ON DELETE SET NULL
        , creation_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        , title TEXT
        , content TEXT
        , main_picture TEXT
        , pictures TEXT ARRAY
        , is_published BOOl
        )|]

createNewsTagTable conn = do
  execute_
    conn
    [sql| CREATE TABLE news_tag
        ( news_id INTEGER REFERENCES news (id) ON DELETE CASCADE
        , tag_id INTEGER REFERENCES tag (id) ON DELETE CASCADE
        , PRIMARY KEY (news_id, tag_id)
        )|]

createCommentTable conn = do
  execute_
    conn
    [sql| CREATE TABLE comment
        ( id SERIAL UNIQUE PRIMARY KEY 
        , user_id INTEGER REFERENCES user_account (id) ON DELETE CASCADE
        , news_id INTEGER REFERENCES news (id) ON DELETE CASCADE
        , creation_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        , content TEXT
        )|]


initDatabase conn = sequence_ $ 
  [ createUserTable
  , createAuthorTable
  , createUserTokenTable
  , createCategoryTable
  , createTagTable
  , createNewsTable
  , createNewsTagTable
  , createCommentTable
  ] <*>
  pure conn
