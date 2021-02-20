{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module DB where
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Control.Applicative
import Control.Monad
import Data.String
import qualified System.Directory as D
import qualified Data.Text as T
import Database.SQLite.Simple.QQ (sql)


dbname :: String
dbname = "qzl.db"

createIfNotExist :: IO ()
createIfNotExist = do
  exist <- D.doesFileExist dbname
  if exist then do
    putStrLn "DB have been exist"
  else do
     create 
     putStrLn "DB created successfully"

create :: IO ()
create = do
  conn <- open dbname
  forM_ querys (execute_ conn)
  close conn

querys :: [Query]
querys = 
  [ [sql| create table test1 (a TEXT) |]
  , [sql| create table test2 (b TEXT) |]
  ]