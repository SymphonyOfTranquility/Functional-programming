{-# LANGUAGE OverloadedStrings #-}
module AuthorsTable where

import Data.Text as T ( Text )
import Database.MySQL.Base
import qualified System.IO.Streams as Streams
import MySQLConnector (getRidOfStream, errorOnExistence)


-- return list of all authors
getAllAuthors :: MySQLConn -> IO [[MySQLValue ]]
getAllAuthors conn = getRidOfStream ( query_ conn "SELECT * FROM authors;")

-- get author from table of authors by email
getAuthorByEmail :: MySQLConn -> T.Text -> IO  [[MySQLValue]]
getAuthorByEmail conn email = getRidOfStream 
    (query conn "SELECT * FROM authors WHERE email = ?;" [MySQLText email])

-- get author from table of authors by surname
getAuthorBySurname :: MySQLConn -> T.Text -> IO  [[MySQLValue]]
getAuthorBySurname conn surname = getRidOfStream 
    (query conn "SELECT * FROM authors WHERE surname = ?;" [MySQLText surname])

-- get author from table of authors by name and surname
getAuthorByNameSurname :: MySQLConn -> T.Text -> T.Text -> IO [[MySQLValue]]
getAuthorByNameSurname conn name surname  = getRidOfStream 
    (query conn "SELECT * FROM authors WHERE name = ? and surname = ?;" 
    [MySQLText name, MySQLText surname])

-- add author to table of authors
addAuthor :: MySQLConn -> T.Text -> T.Text -> T.Text -> IO OK
addAuthor conn name surname email = do
    valsEmail <- getAuthorByEmail conn email
    valsSurname <- getAuthorByNameSurname conn name surname
    if null valsEmail && null valsSurname 
    then execute conn
            "INSERT INTO authors(name, surname, email) \
            \VALUES (?, ?, ?) ;" [MySQLText name, MySQLText surname, MySQLText email]
    else return errorOnExistence

-- update surname in table of authors
updateAuthorSurname :: MySQLConn -> T.Text -> T.Text -> IO OK
updateAuthorSurname conn email surname = do
    vals  <- getAuthorByEmail conn email
    if null vals 
    then return errorOnExistence
    else execute conn
            "UPDATE authors \
            \SET surname = ? \
            \WHERE email = ?;" [MySQLText surname, MySQLText email]

-- update email in table of users
updateAuthorEmail :: MySQLConn -> T.Text -> T.Text -> IO OK
updateAuthorEmail conn email newEmail = do
    vals <- getAuthorByEmail conn email
    valsNew <- getAuthorByEmail conn newEmail
    if not (null vals) && null valsNew
    then execute conn
            "UPDATE authors \
            \SET email = ? \
            \WHERE email = ?;" [MySQLText newEmail, MySQLText email]
    else return errorOnExistence

