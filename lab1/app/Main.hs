{-# LANGUAGE OverloadedStrings #-}
module Main where

import MySQLConnector 
import AuthorsTable 

main :: IO ()
main = do
    conn <- connectDB 

    print =<< getDBName conn

    deployDB conn
    
    print =<< showTables conn

    closeDB conn

