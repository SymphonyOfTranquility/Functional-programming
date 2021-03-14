{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified AuthorsTable as AuthorT
    ( getAllAuthors, getAuthorByEmail, getAuthorBySurname, getAuthorByNameSurname, 
      addAuthor, 
      updateAuthorSurname, updateAuthorEmail,
      deleteAuthor ) 

import qualified UsersTable as UserT
    ( getAllUsers, getUser,
      addUser,
      updateUserName, updateUserPassword, updateUserEmail,
      deleteUser )

import qualified ResourcesTable as ResourceT
    ( getAllResources, getResourceByName, getResourceByLink, getResourceById, getResourceByType,
      addResource,
      updateResourceName, updateResourceType, updateResourceAnnotation, 
      deleteResource )

import qualified AuthorOwnsTable as AuthorOwnT
    ( getAllAuthorOwns, getAuthorOwn, getResourceManagedBy,
      addAuthorResourceRecord,
      deleteAuthor, deleteResource, deleteRecord      
    )

import Data.Time.Calendar as C 
import MySQLConnector ( connectDB, getDBName, deployDB, closeDB, showTables ) 

main :: IO ()
main = do

    conn <- connectDB 

    print =<< getDBName conn

    deployDB conn

    print =<< showTables conn

    closeDB conn

