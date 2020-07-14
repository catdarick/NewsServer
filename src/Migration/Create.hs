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
        , second_name TEXT
        , picture TEXT
        , creation_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        , is_admin BOOL
        )|]

createAuthorTable conn = do
  execute_
    conn
    [sql| CREATE TABLE author
        ( id SERIAL UNIQUE PRIMARY KEY
        , user_id INTEGER UNIQUE REFERENCES user_account (id)
        , description TEXT
        )|]

createUserTokenTable conn = do
  execute_
    conn
    [sql| CREATE TABLE user_token
        ( user_id INTEGER UNIQUE REFERENCES user_account (id)
        , token TEXT
        )|]

createCategoryTable conn = do
  execute_
    conn
    [sql| CREATE TABLE category
        ( id SERIAL UNIQUE PRIMARY KEY
        , parent_id INTEGER REFERENCES category (id)
        , name TEXT
        )|]

createTagTable conn = do
  execute_
    conn
    [sql| CREATE TABLE tag
        ( id SERIAL UNIQUE PRIMARY KEY
        , title TEXT
        )|]

createNewsTable conn = do
  execute_
    conn
    [sql| CREATE TABLE news
        ( id SERIAL UNIQUE PRIMARY KEY
        , author_id INTEGER REFERENCES author (id)
        , category_id INTEGER REFERENCES category (id)
        , creationTime TIMESTAMP DEFAULT CURRENT_TIMESTAMP
        , title TEXT
        , content TEXT
        , main_picture TEXT
        , pictures TEXT ARRAY
        )|]

createNewsTagTable conn = do
  execute_
    conn
    [sql| CREATE TABLE news_tag
        ( news_id INTEGER REFERENCES news (id)
        , tag_id INTEGER REFERENCES tag (id)
        , PRIMARY KEY (news_id, tag_id)
        )|]

createCommentTable conn = do
  execute_
    conn
    [sql| CREATE TABLE comment
        ( id SERIAL UNIQUE PRIMARY KEY
        , user_id INTEGER REFERENCES user_account (id)
        , content TEXT
        )|]

createNewsCommentTable conn = do
  execute_
    conn
    [sql| CREATE TABLE news_comment
        ( news_id INTEGER REFERENCES news (id)
        , comment_id INTEGER REFERENCES comment (id)
        , PRIMARY KEY (news_id, comment_id)
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
  , createNewsCommentTable
  ] <*>
  pure conn
