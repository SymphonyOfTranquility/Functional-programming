{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified AuthorsTable as AuthorT
    ( getAllAuthors, getAuthorByEmail, getAuthorBySurname, getAuthorByNameSurname, 
      addAuthor, 
      updateAuthorSurname, updateAuthorEmail ) 

import qualified UsersTable as UserT
    ( getAllUsers, getUser,
      addUser,
      updateUserName, updateUserPassword, updateUserEmail )

import qualified ResourcesTable as ResourceT
    ( getAllResources, getResourceByName, getResourceByLink, getResourceById, getResourceByType,
      addResource,
      updateResourceName, updateResourceType, updateResourceAnnotation )

import Data.Time.Calendar as C 
import MySQLConnector ( connectDB, getDBName, deployDB, closeDB, showTables ) 

-- TODO change query creation q = Query (BtSt.pack (string_value)) !! string_value can be changable (and be concatenation)
-- TODO Generalization of functions (get, update, add???)
-- TODO Add deletion of values

main :: IO ()
main = do

    conn <- connectDB 

    print =<< getDBName conn

    deployDB conn

    print =<< showTables conn
    
    closeDB conn

