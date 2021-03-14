{-# LANGUAGE OverloadedStrings #-}
module MySQLConnector where

import Data.Text as T ( Text )
import Database.MySQL.Base
import qualified System.IO.Streams as Streams
import qualified Data.ByteString.Lazy.Char8 as BtSt ( pack )

-- connect to database which is started by docker-compose
connectDB :: IO MySQLConn
connectDB =
    connect
        defaultConnectInfo { ciPort = 3307,
                             ciUser = "user",
                             ciPassword = "pass1234",
                             ciDatabase = "db"
                            }

-- close connection to database
closeDB :: MySQLConn -> IO ()
closeDB = close

-- translate output results into list of values
getRidOfStream :: IO ([ColumnDef], Streams.InputStream [MySQLValue]) -> IO [[MySQLValue]]
getRidOfStream all = do
    (defs, is) <- all
    Streams.toList is

-- get name of database
getDBName :: MySQLConn -> IO [[MySQLValue]]
getDBName conn = getRidOfStream (query_ conn "SELECT DATABASE();")

-- create tables if they doesn't exist
deployDB :: MySQLConn -> IO [OK]
deployDB conn = executeMany_ conn
    "CREATE TABLE IF NOT EXISTS users ( \
        \user_id INT NOT NULL AUTO_INCREMENT, \
        \email VARCHAR(256) NOT NULL, \
        \password VARCHAR(256) NOT NULL, \
        \name VARCHAR(256) NOT NULL, \
        \PRIMARY KEY ( user_id ) \
    \); \

    \CREATE TABLE IF NOT EXISTS authors ( \
        \author_id INT NOT NULL AUTO_INCREMENT, \
        \name VARCHAR(256) NOT NULL, \
        \surname VARCHAR(256) NOT NULL, \
        \email VARCHAR(256) NOT NULL, \
        \PRIMARY KEY ( author_id ) \
    \); \

    \CREATE TABLE IF NOT EXISTS resources ( \
        \resource_id INT NOT NULL AUTO_INCREMENT, \
        \name TEXT NOT NULL, \
        \type ENUM('A', 'B', 'C') NOT NULL, \
        \annotation TEXT NOT NULL, \
        \link TEXT NOT NULL, \
        \purpose TEXT NOT NULL, \
        \open_date DATE, \
        \usage_time INT NOT NULL, \
        \rules TEXT NOT NULL, \
        \statistics INT NOT NULL, \
        \PRIMARY KEY ( resource_id ) \ 
    \); \

    \CREATE TABLE IF NOT EXISTS author_owns ( \
        \record_id INT NOT NULL AUTO_INCREMENT, \
        \author INT NOT NULL, \
        \resource INT NOT NULL, \
        \PRIMARY KEY ( record_id ), \
        \FOREIGN KEY ( author ) REFERENCES authors( author_id ), \
        \FOREIGN KEY ( resource ) REFERENCES resources( resource_id ) \
    \); \
    
    \CREATE TABLE IF NOT EXISTS user_owns ( \
        \record_id INT NOT NULL AUTO_INCREMENT, \
        \user INT NOT NULL, \
        \resource INT NOT NULL, \
        \usage_start DATE NOT NULL, \
        \PRIMARY KEY ( record_id ), \
        \FOREIGN KEY ( user ) REFERENCES users( user_id ), \
        \FOREIGN KEY ( resource ) REFERENCES resources( resource_id ) \
    \);"

-- return list of tables
showTables :: MySQLConn -> IO [[MySQLValue]]
showTables conn = getRidOfStream (query_ conn "SHOW TABLES;")

-- convert to query
toQuery :: String -> Query
toQuery = Query . BtSt.pack

-- return describe table
describeTable :: MySQLConn -> String  -> IO [[MySQLValue]]
describeTable conn tableName = getRidOfStream (query_ conn (toQuery ("DESCRIBE " ++ tableName ++ ";")))

-- delete table from database
dropTable :: MySQLConn -> String -> IO OK
dropTable conn tableName = execute_ conn (toQuery ("DROP TABLE " ++ tableName ++";"))

-- error status if object not exists
errorOnExistence :: OK
errorOnExistence = OK (-100) (-100) 0 0

-- get all values from table
getAllValues :: MySQLConn -> String -> IO [[MySQLValue]]
getAllValues conn tableName = getRidOfStream ( query_ conn (toQuery("SELECT * FROM " ++ tableName ++ ";")))

-- generate where query
generateWhere :: [String] -> String
generateWhere [] = ""
generateWhere [x] = x ++ " = ?;"
generateWhere (x:xs) = x ++ " = ? and " ++ generateWhere xs

-- get any value by compare from table
getValue :: MySQLConn -> String -> [String] -> [MySQLValue] -> IO [[MySQLValue]]
getValue conn tableName fieldNames fieldValues =
    getRidOfStream (query conn (
        toQuery ("SELECT * FROM " ++ tableName ++ " WHERE " ++ generateWhere fieldNames))
        fieldValues)

-- update field for any table
updateField :: MySQLConn -> String -> String -> String -> MySQLValue -> T.Text -> IO OK
updateField conn tableName fieldToUpdate fieldToCompare updateValue compareValue = do
    vals <- getValue conn tableName [fieldToCompare] [MySQLText compareValue]
    if null vals
    then return errorOnExistence
    else execute conn (toQuery (
        "UPDATE " ++ tableName ++ " \
        \SET " ++ fieldToUpdate ++ " = ? \
        \WHERE " ++ fieldToCompare ++ " = ?;"))
        [updateValue, MySQLText compareValue]

-- update field for key in any table
updateKeyField :: MySQLConn -> String -> String -> T.Text -> T.Text -> IO OK
updateKeyField conn tableName fieldToUpdate updateValue compareValue = do
    vals <- getValue conn tableName [fieldToUpdate] [MySQLText compareValue]
    valsNew <- getValue conn tableName [fieldToUpdate] [MySQLText updateValue]
    if null vals || not (null valsNew)
    then return errorOnExistence
    else execute conn (toQuery (
        "UPDATE " ++ tableName ++ " \
        \SET " ++ fieldToUpdate ++ " = ? \
        \WHERE " ++ fieldToUpdate ++ " = ?;"))
        [MySQLText updateValue, MySQLText compareValue]


-- generate where query
generateInsert :: [String] -> String
generateInsert [] = " "
generateInsert [x] = x
generateInsert (x:xs) = x ++ ", " ++ generateInsert xs

generateInsertValues :: Int -> String
generateInsertValues n
    | n == 0 = " "
    | n == 1 = "?"
    | n > 1  = "?, " ++ generateInsertValues (n-1)

-- add value to table
addValue :: MySQLConn -> String -> [String] -> [MySQLValue] -> IO OK
addValue conn tableName fieldNames fieldValues = do
    vals <- getValue conn tableName [head fieldNames] [head fieldValues]
    if null vals
    then execute conn
            (toQuery ("INSERT INTO " ++ tableName ++ " (" ++ generateInsert fieldNames ++ ") " ++
                      "VALUES (" ++ generateInsertValues (length fieldValues) ++ ") ;"))
            fieldValues
    else return errorOnExistence
