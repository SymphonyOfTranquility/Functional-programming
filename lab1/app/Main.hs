{-# LANGUAGE OverloadedStrings #-}
module Main where

import MySQLConnector 

main :: IO ()
main = do
    conn <- connectDB 

    vals <- getDBName conn
    print vals

    deployDB conn
    
    vals <- showTables conn
    print vals

    closeDB conn

