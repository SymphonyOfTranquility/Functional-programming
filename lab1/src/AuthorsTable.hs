{-# LANGUAGE OverloadedStrings #-}
module AuthorsTable where

import Data.Text as T ( Text )
import Database.MySQL.Base
import MySQLConnector (updateField, updateKeyField, getAllValues, getValue, addValue, deleteValue)

tableName :: String 
tableName = "authors"

-- return list of all authors
getAllAuthors :: MySQLConn -> IO [[MySQLValue ]]
getAllAuthors conn = getAllValues conn tableName

-- get author from table of authors by email
getAuthorByEmail :: MySQLConn -> T.Text -> IO  [[MySQLValue]]
getAuthorByEmail conn email = getValue conn tableName ["email"] [MySQLText email]

-- get author from table of authors by surname
getAuthorBySurname :: MySQLConn -> T.Text -> IO  [[MySQLValue]]
getAuthorBySurname conn surname = getValue conn tableName ["surname"] [MySQLText surname]

-- get author from table of authors by name and surname
getAuthorByNameSurname :: MySQLConn -> T.Text -> T.Text -> IO [[MySQLValue]]
getAuthorByNameSurname conn name surname = getValue conn tableName ["name", "surname"] [MySQLText name, MySQLText surname]

-- add author to table of authors
addAuthor :: MySQLConn -> T.Text -> T.Text -> T.Text -> IO OK
addAuthor conn email name surname = 
    addValue conn tableName ["email", "name", "surname"] 
                            [MySQLText email, MySQLText name, MySQLText surname]

-- update surname in table of authors
updateAuthorSurname :: MySQLConn -> T.Text -> T.Text -> IO OK
updateAuthorSurname conn email surname = 
    updateField conn tableName "surname" "email" (MySQLText surname) email
    
-- update email in table of authors
updateAuthorEmail :: MySQLConn -> T.Text -> T.Text -> IO OK
updateAuthorEmail conn email newEmail = 
    updateKeyField conn tableName "email" newEmail email

-- delete author from table
deleteAuthor :: MySQLConn -> T.Text -> IO OK
deleteAuthor conn email = deleteValue conn tableName "email" (MySQLText email)

    