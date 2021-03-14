{-# LANGUAGE OverloadedStrings #-}
module UsersTable where

import Data.Text as T ( Text )
import Database.MySQL.Base
import MySQLConnector (updateField, updateKeyField, getAllValues, getValue, addValue, deleteValue)

tableName :: String 
tableName = "users"

-- return list of all users
getAllUsers :: MySQLConn -> IO [[MySQLValue ]]
getAllUsers conn = getAllValues conn tableName

-- get user from table of users
getUser :: MySQLConn -> T.Text -> IO  [[MySQLValue]]
getUser conn email = getValue conn tableName ["email"] [MySQLText email]

-- add new user to table of users
addUser :: MySQLConn -> T.Text -> T.Text -> T.Text -> IO OK
addUser conn email username password = 
        addValue conn tableName ["email", "name", "password"] [MySQLText email, MySQLText username, MySQLText password]

-- update username in table of users
updateUserName :: MySQLConn -> T.Text -> T.Text -> IO OK
updateUserName conn email username = 
        updateField conn tableName "name" "email" (MySQLText username) email

-- update password in table of users
updateUserPassword :: MySQLConn -> T.Text -> T.Text -> IO OK
updateUserPassword conn email password = 
        updateField conn tableName "password" "email" (MySQLText password) email

-- update email in table of users
updateUserEmail :: MySQLConn -> T.Text -> T.Text -> IO OK
updateUserEmail conn email newEmail =
        updateKeyField conn tableName "email" newEmail email

-- delete user from table
deleteUser :: MySQLConn -> T.Text -> IO OK
deleteUser conn email = deleteValue conn tableName "email" (MySQLText email)
