{-# LANGUAGE OverloadedStrings #-}

module Api.Methods.Errors where

import           Data.ByteString (ByteString)

smth :: ByteString
smth = "Something goes wrong"

loginBusy :: ByteString
loginBusy = "Login is busy, try another"

badPassword :: ByteString
badPassword = "Incorrect login or password"

badToken :: ByteString
badToken = "Bad token"

alreadyAuthor :: ByteString
alreadyAuthor = "This user is already an author or doesn't exists"

notAuthor :: ByteString
notAuthor = "You are not an author"

noParrent :: ByteString
noParrent = "Parrent with this id doesnt exists"

noCategory :: ByteString
noCategory = "Category with this id doesnt exists"

noDraft :: ByteString
noDraft = "Draft with this id doesnt exists"

noNews :: ByteString
noNews = "News with this id doesnt exists"

tagExists :: ByteString
tagExists = "This tag already exists"

badTags :: ByteString
badTags = "Duplicates tag_id or doesnt exists"
