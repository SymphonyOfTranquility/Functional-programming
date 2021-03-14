{-# LANGUAGE OverloadedStrings #-}
module UserOwnsTable where

import qualified Data.Time.Calendar as C ( Day )
import Data.Text as T ( Text )
import Data.Int (Int32)
import Database.MySQL.Base
import MySQLConnector (updateField, updateKeyField, getAllValues, getValue, addValue, deleteValue, addValueConnective)

tableName :: String 
tableName = "user_owns"

-- return list of all user_owns
getAllUserOwns :: MySQLConn -> IO [[MySQLValue]]
getAllUserOwns conn = getAllValues conn tableName

-- get users resources from table of user_owns
getUserOwn :: MySQLConn -> Int32 -> IO [[MySQLValue]]
getUserOwn conn userId =
    getValue conn tableName ["user"] [MySQLInt32 userId]

-- get whom resource belongs
getResourceManagedBy :: MySQLConn -> Int32 -> IO [[MySQLValue]]
getResourceManagedBy conn resourceId = 
    getValue conn tableName ["resource"] [MySQLInt32 resourceId]

-- add new record
addUserResourceRecord :: MySQLConn -> Int32 -> Int32 -> C.Day -> IO OK
addUserResourceRecord conn userId resourceId usageStart = 
        addValueConnective conn tableName ["user", "resource", "usage_start"] 
                                          [MySQLInt32 userId, MySQLInt32 resourceId, MySQLDate usageStart]

-- delete user from table
deleteUser :: MySQLConn -> Int32 -> IO OK
deleteUser conn userId = deleteValue conn tableName ["user"] [MySQLInt32 userId]

-- delete resource from table
deleteResource :: MySQLConn -> Int32 -> IO OK
deleteResource conn resourceId = deleteValue conn tableName ["resource"] [MySQLInt32 resourceId]

-- delete 1 record
deleteRecord :: MySQLConn -> Int32 -> Int32 -> IO OK
deleteRecord conn userId resourceId = 
    deleteValue conn tableName ["user", "resource"] 
                               [MySQLInt32 userId, MySQLInt32 resourceId]