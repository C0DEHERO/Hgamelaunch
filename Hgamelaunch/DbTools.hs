{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Hgamelaunch.DbTools
       ( makeDb,
         insertUser,
         updateUser,
         updateMaybeUser,
         fetchUserFromDb,
         UserField(UserField),
         userIdNum,
         userUsername,
         userPassword,
         userEmail,
         userAdmin,
         userDebugger
         ) where

import Data.Text
import Control.Applicative
import Database.SQLite.Simple
import Control.Lens

data UserField = UserField { _userIdNum    :: Int
                           , _userUsername :: Text
                           , _userPassword :: Text
                           , _userEmail    :: Text
                           , _userAdmin    :: Bool
                           , _userDebugger :: Bool
                           } deriving (Show)

makeLenses ''UserField

instance FromRow UserField where
  fromRow = UserField <$> field <*> field <*> field <*> field <*> field <*> field

makeDb :: Connection -> Bool -> IO ()
makeDb conn False = do
  execute conn "CREATE TABLE users(id INTEGER PRIMARY KEY,username VARCHAR NOT NULL,password VARCHAR NOT NULL,email VARCHAR NOT NULL,admin BOOLEAN NOT NULL,debugger BOOLEAN NOT NULL,CONSTRAINT username_key UNIQUE (username))" ()
  insertUser conn (UserField 0 "admin" "admin" "admin@server.org" True False)
makeDb _ True = return ()

insertUser :: Connection -> UserField -> IO ()
insertUser conn (UserField _ username password email admin debugger) = do
  execute conn "INSERT INTO users (username, password, email, admin, debugger) VALUES (?, ?, ?, ?, ?)" (username, password, email, admin :: Bool, debugger :: Bool)

updateUser :: Connection -> UserField -> IO ()
updateUser conn (UserField idNum username password email admin debugger) = do
  execute conn "UPDATE users SET username=?, password=?, email=?, admin=?, debugger=? WHERE id=?" (username, password, email, admin :: Bool, debugger :: Bool, idNum :: Int)

updateMaybeUser :: Connection -> Maybe UserField -> IO ()
updateMaybeUser conn (Just user) = do
  updateUser conn user
updateMaybeUser _ Nothing = do
  return ()

fetchUserFromDb :: Connection -> Text -> IO (Maybe UserField)
fetchUserFromDb conn username = do
  xs <- query conn "SELECT * FROM users WHERE username like ? LIMIT 1" (Only username)
--  return $ head xs
  case xs of [] -> return Nothing
             x:[] -> return (Just x)
             _ -> return Nothing
