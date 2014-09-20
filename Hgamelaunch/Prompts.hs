module Hgamelaunch.Prompts
       ( askPrompt,
         loginPrompt,
         registerPrompt,
         passwdPrompt,
         emailPrompt,
         addUserPrompt
       ) where

import Hgamelaunch.DbTools
import Hgamelaunch.Banners
import Data.Char (toUpper)
import Data.Text (Text, pack)
import Data.Maybe (isJust)
import Control.Applicative

askPrompt :: String -> IO (Maybe Text)
askPrompt str = do
  putStrLn $ "Please input " ++ str ++ "."
  result <- getLine
  return (if not (null result) then Just (pack result) else Nothing)

ynPrompt :: String -> IO Bool
ynPrompt str = do
  putStrLn $ str
  result <- getLine
  case (ynStrToBool result) of
    (Just ch) -> return ch
    Nothing -> ynPrompt str

ynStrToBool :: String -> Maybe Bool
ynStrToBool str
  | map toUpper str `elem` ["Y","YES","TRUE"] = Just True
  | map toUpper str `elem` ["N", "NO", "FALSE"] = Just False
  | otherwise = Nothing

loginPrompt :: IO (Maybe (Text,Text))
loginPrompt = do
  user <- askPrompt "your username"
  pass <- askPrompt "your password"
  return $ (,) <$> user <*> pass

registerPrompt :: IO (Maybe (Text,Text,Text))
registerPrompt = do
  user <- askPrompt "your username"
  pass <- passwdPrompt "your password"
  email <- askPrompt "your email"
  return $ (,,) <$> user <*> pass <*> email
  

passwdPrompt :: String -> IO (Maybe Text) 
passwdPrompt str = do
  pass <- askPrompt str
  pass2 <- askPrompt (str ++ "again")
  return (checkPass pass pass2)
    where checkPass pass pass2
            | pass == pass2 = pass
            | otherwise = Nothing

emailPrompt :: IO (Maybe Text)
emailPrompt = do
  email <- askPrompt "your email"
  return (email)

addUserPrompt :: IO (Maybe UserField)
addUserPrompt = do
  user <- askPrompt "username"
  pass <- passwdPrompt "your password"
  email <- askPrompt "email"
  admin <- ynPrompt "admin?"
  debugger <- ynPrompt "debugger?"
  return $ UserField <$> Just 0 <*> user <*> pass <*> email <*> Just admin <*> Just debugger

