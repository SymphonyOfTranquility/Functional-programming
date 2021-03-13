{-# LANGUAGE OverloadedStrings #-}
module Main where

import MySQLConnector 
import qualified System.IO.Streams as Streams
import Database.MySQL.Base

main :: IO ()
main = do
    conn <- connectDB 

    (defs, is) <- getDBName conn
    print =<< Streams.toList is

    deployDB conn
    
    (def, is) <- showTables conn
    print =<< Streams.toList is

    closeDB conn

