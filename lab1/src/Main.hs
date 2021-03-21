{-# LANGUAGE OverloadedStrings #-}
module Main where

import MySQLConnector ( connectDB, deployDB, closeDB, getDBName, Table (getAllValues, getValue, printInfo, addValue, deleteValue) ) 
import qualified UsersTable as UT
    ( UsersInfo(UsersInfo, tableName, fieldNames), emptyUserStruct ) 
import qualified AuthorsTable as AT
    ( AuthorsInfo(AuthorsInfo, ids, names, surnames, emails, tableName, fieldNames),
      emptyAuthorStruct ) 

import qualified ResourcesTable as RT
    ( ResourcesInfo(ResourcesInfo, ids, names, types, annotations,
                    links, purposes, openDate, usageTime, rules, statistics, tableName,
                    fieldNames),
      emptyResourceStruct ) 
import Data.Time.Calendar as C ( Day, fromGregorian )
import qualified AuthorsToResources as A2RT
    ( AuthorToResources(AuthorToResources, fieldNames, ids, authorIds,
                        resourceIds, tableName),
      emptyAuthorToResourceStruct )
import UsersToResources
-- import qualified UserInteraction as UI ( doAction )


main :: IO ()
main = do

    conn <- connectDB 
    deployDB conn

    print =<< getDBName conn
    -- UI.doAction conn

    {-
    let userInfo = UsersInfo 
                    (tableName emptyUserStruct) 
                        (fieldNames emptyUserStruct) [1] [] [] []
    printInfo userInfo
    res <- getAllValues userInfo conn
    printInfo res
    res <- getValue userInfo conn
    printInfo res
    -}
    {-
    let author = AT.AuthorsInfo {
                    AT.tableName = AT.tableName AT.emptyAuthorStruct,
                    AT.fieldNames = AT.fieldNames AT.emptyAuthorStruct,
                    AT.ids = [],
                    AT.names = [],
                    AT.surnames = ["Cat"],
                    AT.emails = []
                }
    printInfo author
    -- res <- addValue author conn
    -- print res
    res <- getAllValues author conn
    printInfo res
    -- res <- deleteValue author conn
    -- print res

    res <- getValue author conn
    printInfo res
    -}
    {-
    let resource = RT.ResourcesInfo {
            RT.tableName = RT.tableName RT.emptyResourceStruct,
            RT.fieldNames = RT.fieldNames RT.emptyResourceStruct,
            RT.ids = [],
            RT.names = [],
            RT.types = [],
            RT.annotations = [],
            RT.links = [],
            RT.purposes = [],
            RT.openDate = [fromGregorian 2020 10 10],
            RT.usageTime = [],
            RT.rules = [],
            RT.statistics = []
    }
    
    res <- getValue resource conn
    -}

    {-
    let authorToResource = A2RT.AuthorToResources{ 
        A2RT.tableName = A2RT.tableName A2RT.emptyAuthorToResourceStruct,
        A2RT.fieldNames = A2RT.fieldNames A2RT.emptyAuthorToResourceStruct,
        A2RT.ids = [],
        A2RT.authorIds = [3],
        A2RT.resourceIds = [2]
    }
    res <- getAllValues authorToResource conn
    printInfo res conn
    print =<< addValue authorToResource conn

    res <- getAllValues authorToResource conn
    printInfo res conn
    -}

    let userToResource = UserToResources {
            tableName = tableName emptyUserToResourceStruct ,
            fieldNames = fieldNames emptyUserToResourceStruct ,
            ids = [],
            userIds = [],
            resourceIds = [],
            usageStart = []
        }

    res <- getAllValues userToResource conn
    printInfo res conn
    closeDB conn

  

