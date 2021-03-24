{-# LANGUAGE OverloadedStrings #-}
module Main where

import MySQLConnector ( connectDB, deployDB, closeDB, getDBName, Table (getAllValues, getValue, printInfo, addValue, deleteValue, drop) )
import qualified UserInteraction as UI ( doAction, dropAll )


main :: IO ()
main = do

    conn <- connectDB
    deployDB conn

    print =<< getDBName conn

    UI.doAction conn
    -- UI.dropAll conn
    
    closeDB conn
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
    {-
    let userToResource = U2RT.UserToResources {
            U2RT.tableName = U2RT.tableName U2RT.emptyUserToResourceStruct ,
            U2RT.fieldNames = U2RT.fieldNames U2RT.emptyUserToResourceStruct ,
            U2RT.ids = [],
            U2RT.userIds = [],
            U2RT.resourceIds = [],
            U2RT.usageStart = []
        }

    res <- getAllValues userToResource conn
    printInfo res conn

    -}



