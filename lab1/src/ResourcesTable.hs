{-# LANGUAGE OverloadedStrings #-}
module ResourcesTable where

import Data.Text as T ( Text )
import Data.Time.Calendar as C ( Day )
import Database.MySQL.Base
import MySQLConnector (updateField, updateKeyField, getAllValues, getValue, addValue, deleteValue)
import Data.Int (Int32)

tableName :: String 
tableName = "resources"

-- return list of all authors
getAllResources :: MySQLConn -> IO [[MySQLValue]]
getAllResources conn = getAllValues conn tableName

-- get resource from table of resources by name
getResourceByName :: MySQLConn -> T.Text -> IO [[MySQLValue]]
getResourceByName conn name = getValue conn tableName ["name"] [MySQLText name]

-- get resource from table of resources by link
getResourceByLink :: MySQLConn -> T.Text -> IO [[MySQLValue]]
getResourceByLink conn link =  getValue conn tableName ["link"] [MySQLText link]

-- get resource from table of resources by id
getResourceById :: MySQLConn -> Int32 -> IO [[MySQLValue]]
getResourceById conn resourceId =  getValue conn tableName ["resource_id"] [MySQLInt32 resourceId] 

-- get resources from table of resources by type
getResourceByType :: MySQLConn -> T.Text -> IO [[MySQLValue]]
getResourceByType conn resourceType =  getValue conn tableName ["type"] [MySQLText resourceType] 

-- add resource to table of resources
addResource :: MySQLConn -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> C.Day -> Int32 -> T.Text -> Int32 -> IO OK
addResource conn name resourceType annotation link purpose openDate usageTime rules statistics = 
    addValue conn tableName ["link", "name", "type", "annotation", "purpose", "open_date", "usage_time", "rules", "statistics"] 
                            [MySQLText link, MySQLText name, MySQLText resourceType, MySQLText annotation, MySQLText purpose,
                             MySQLDate openDate, MySQLInt32 usageTime, MySQLText rules, MySQLInt32 statistics]
    
-- update name in table of resources
updateResourceName :: MySQLConn -> T.Text -> T.Text -> IO OK
updateResourceName conn name newName = 
    updateKeyField conn tableName "name" newName name

-- update type in table of resources
updateResourceType :: MySQLConn -> T.Text -> T.Text -> IO OK
updateResourceType conn name newType = 
    updateField conn tableName "type" "name" (MySQLText newType) name

-- update type in table of resources
updateResourceAnnotation :: MySQLConn -> T.Text -> T.Text -> IO OK
updateResourceAnnotation conn name annotation = 
    updateField conn tableName "annotation" "name" (MySQLText annotation) name

-- delete resource from table
deleteResource :: MySQLConn -> T.Text -> IO OK
deleteResource conn name = deleteValue conn tableName "name" (MySQLText name)

