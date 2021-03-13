{-# LANGUAGE OverloadedStrings #-}
module UsersTable where

import Data.Text as T ( Text )
import Database.MySQL.Base
import qualified System.IO.Streams as Streams
import MySQLConnector (getRidOfStream, errorOnExistence)


-- return list of all users
getAllUsers :: MySQLConn -> IO [[MySQLValue ]]
getAllUsers conn = getRidOfStream ( query_ conn "SELECT * FROM users;")

-- get user from table of users
getUser :: MySQLConn -> T.Text -> IO  [[MySQLValue]]
getUser conn email = getRidOfStream (query conn "SELECT * FROM users WHERE email = ?;" [MySQLText email])

-- add new user to table of users
addUser :: MySQLConn -> T.Text -> T.Text -> T.Text -> IO OK
addUser conn username email password = do
    vals <- getUser conn email
    if null vals 
    then execute conn
            "INSERT INTO users(email, password, name) \
            \VALUES (?, ?, ?) ;" [MySQLText email, MySQLText password, MySQLText username]
    else return errorOnExistence

-- update username in table of users
updateUserName :: MySQLConn -> T.Text -> T.Text -> IO OK
updateUserName conn email username = do
    vals  <- getUser conn email
    if null vals 
    then return errorOnExistence
    else execute conn
            "UPDATE users \
            \SET name = ? \
            \WHERE email = ?;" [MySQLText username, MySQLText email]

-- update password in table of users
updateUserPassword :: MySQLConn -> T.Text -> T.Text -> IO OK
updateUserPassword conn email password = do
    vals <- getUser conn email
    if null vals 
    then return errorOnExistence
    else execute conn
            "UPDATE users \
            \SET password = ? \
            \WHERE email = ?;" [MySQLText password, MySQLText email]

-- update email in table of users
updateUserEmail :: MySQLConn -> T.Text -> T.Text -> IO OK
updateUserEmail conn email newEmail = do
    vals <- getUser conn email
    valsNew <- getUser conn newEmail
    if not (null vals) && null valsNew
    then execute conn
            "UPDATE users \
            \SET email = ? \
            \WHERE email = ?;" [MySQLText newEmail, MySQLText email]
    else return errorOnExistence
