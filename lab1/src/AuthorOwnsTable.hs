{-# LANGUAGE OverloadedStrings #-}
module AuthorOwnsTable where

import Data.Text as T ( Text )
import Data.Int (Int32)
import Database.MySQL.Base
import MySQLConnector (updateField, updateKeyField, getAllValues, getValue, addValue, deleteValue, addValueConnective)

tableName :: String 
tableName = "author_owns"

-- return list of all author_owns
getAllAuthorOwns :: MySQLConn -> IO [[MySQLValue ]]
getAllAuthorOwns conn = getAllValues conn tableName

-- get authors resources from table of author_owns
getAuthorOwn :: MySQLConn -> Int32 -> IO [[MySQLValue]]
getAuthorOwn conn authorId =
    getValue conn tableName ["author"] [MySQLInt32 authorId]

-- get whom resource belongs
getResourceManagedBy :: MySQLConn -> Int32 -> IO [[MySQLValue]]
getResourceManagedBy conn resourceId = 
    getValue conn tableName ["resource"] [MySQLInt32 resourceId]

-- add new record
addAuthorResourceRecord :: MySQLConn -> Int32 -> Int32 -> IO OK
addAuthorResourceRecord conn authorId resourceId = 
        addValueConnective conn tableName ["author", "resource"] [MySQLInt32 authorId, MySQLInt32 resourceId]

-- delete author from table
deleteAuthor :: MySQLConn -> Int32 -> IO OK
deleteAuthor conn authorId = deleteValue conn tableName ["author"] [MySQLInt32 authorId]

-- delete resource from table
deleteResource :: MySQLConn -> Int32 -> IO OK
deleteResource conn resourceId = deleteValue conn tableName ["resource"] [MySQLInt32 resourceId]

-- delete 1 record
deleteRecord :: MySQLConn -> Int32 -> Int32 -> IO OK
deleteRecord conn authorId resourceId = 
    deleteValue conn tableName ["author", "resource"] 
                               [MySQLInt32 authorId, MySQLInt32 resourceId]