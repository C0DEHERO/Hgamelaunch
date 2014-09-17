{-# LANGUAGE OverloadedStrings #-}
module Hgamelaunch.DbTools
       ( makeDb,
         insertUser,
         updateUser,
         updateMaybeUser,
         fetchUserFromDb,
         UserField(UserField)
         ) where

import Data.Text
import Control.Applicative
import Database.SQLite.Simple

data UserField = UserField Int Text Text Text Bool Bool deriving (Show)

instance FromRow UserField where
  fromRow = UserField <$> field <*> field <*> field <*> field <*> field <*> field

makeDb :: Connection -> IO ()
makeDb conn = do
  execute conn "CREATE TABLE users(id INTEGER PRIMARY KEY,username VARCHAR NOT NULL,password VARCHAR NOT NULL,email VARCHAR NOT NULL,admin BOOLEAN NOT NULL,debugger BOOLEAN NOT NULL,CONSTRAINT username_key UNIQUE (username))" ()
  insertUser conn "codehero" "pass" "codehero@nerdpol.ch" False True
  insertUser conn "hauzer" "password456" "hauzer@gmail.com" False False
  insertUser conn "Mortvert" "password789" "mortvert@gmail.com" False True
  insertUser conn "admin" "password" "admin@server.org" True False

insertUser :: Connection -> String -> String -> String -> Bool -> Bool -> IO ()
insertUser conn username password email admin debugger = do
  execute conn "INSERT INTO users (username, password, email, admin, debugger) VALUES (?, ?, ?, ?, ?)" (username, password, email, admin :: Bool, debugger :: Bool)

updateUser :: Connection -> UserField -> Text -> Text -> Text -> Bool -> Bool -> IO ()
updateUser conn (UserField idNum _ _ _ _ _) username password email admin debugger = do
  execute conn "UPDATE users SET username=?, password=?, email=?, admin=?, debugger=? WHERE id=?" (username, password, email, admin :: Bool, debugger :: Bool, idNum :: Int)

updateMaybeUser :: Connection -> Maybe UserField -> Text -> Text -> Text -> Bool -> Bool -> IO ()
updateMaybeUser conn (Just user) username password email admin debugger = do
  updateUser conn user username password email admin debugger
updateMaybeUser _ Nothing _ _ _ _ _ = do
  return ()

fetchUserFromDb :: (FromRow r) => Connection -> String -> IO (Maybe r)
fetchUserFromDb conn username = do
  xs <- query conn "SELECT * FROM users WHERE username like ? LIMIT 1" (Only username)
--  return $ head xs
  case xs of [] -> return Nothing
             x:[] -> return (Just x)
             _ -> return Nothing
