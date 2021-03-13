{-# LANGUAGE OverloadedStrings #-}
module MySQLConnector where

import Data.Text as T ( Text )
import Database.MySQL.Base
import qualified System.IO.Streams as Streams

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

-- return describe table
describeTable :: MySQLConn -> String  -> IO [[MySQLValue]]
describeTable conn tableName 
    | tableName == "users"      = getRidOfStream (query_ conn "DESCRIBE users;")
    | tableName == "authors"    = getRidOfStream (query_ conn "DESCRIBE authors;")
    | tableName == "resources"  = getRidOfStream (query_ conn "DESCRIBE resources;")
    | tableName == "author_owns"= getRidOfStream (query_ conn "DESCRIBE author_owns;")
    | tableName == "user_owns"  = getRidOfStream (query_ conn "DESCRIBE user_owns;")

-- delete table from database
dropTable :: MySQLConn -> String -> IO OK
dropTable conn tableName
    | tableName == "users"      = execute_ conn "DROP TABLE users;"
    | tableName == "authors"    = execute_ conn "DROP TABLE authors;"
    | tableName == "resources"  = execute_ conn "DROP TABLE resources;"
    | tableName == "author_owns"    = execute_ conn "DROP TABLE author_owns;"
    | tableName == "user_owns"    = execute_ conn "DROP TABLE user_owns;"

-- error status if object not exists
errorOnExistence :: OK
errorOnExistence = OK (-100) (-100) 0 0
