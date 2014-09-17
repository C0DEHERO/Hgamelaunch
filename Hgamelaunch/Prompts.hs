module Hgamelaunch.Prompts
       ( askPrompt,
         loginPrompt,
         registerPrompt,
         passwdPrompt,
         emailPrompt,
         addUserPrompt
       ) where

import Data.Char (toUpper)

askPrompt :: String -> IO String
askPrompt str = do
  putStrLn $ "Please input " ++ str ++ "."
  getLine

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

loginPrompt :: IO (String,String)
loginPrompt = do
  user <- askPrompt "your username"
  pass <- askPrompt "your password"
  return (user, pass)

registerPrompt :: IO (Maybe (String,String,String))
registerPrompt = do
  user <- askPrompt "your username"
  pass <- askPrompt "your password"
  pass2 <- askPrompt "your password again"
  email <- askPrompt "your email"
  return (userPass user pass pass2 email)
    where userPass user pass pass2 email
            | pass == pass2 = Just (user, pass, email)
            | otherwise = Nothing

passwdPrompt :: IO (Maybe String) 
passwdPrompt = do
  pass <- askPrompt "password"
  pass2 <- askPrompt "password again"
  return (checkPass pass pass2)
    where checkPass pass pass2
            | pass == pass2 = Just pass
            | otherwise = Nothing

emailPrompt :: IO (Maybe String)
emailPrompt = do
  email <- askPrompt "your email"
  return (Just email)

addUserPrompt :: IO (Maybe (String, String, String, Bool, Bool))
addUserPrompt = do
  user <- askPrompt "username"
  pass <- passwdPrompt
  email <- askPrompt "email"
  admin <- ynPrompt "admin?"
  debugger <- ynPrompt "debugger?"
  return $ checkPass (user, pass, email, admin, debugger)
    where checkPass (user, Just pass, email, admin, debugger) = Just (user, pass, email, admin, debugger)
          checkPass _ = Nothing

{-
modUserPrompt :: IO (Maybe (String, String, String, Bool, Bool))
modUserPrompt = do
  choice <- modWhatPrompt



modWhatPrompt = do
  choice <- askPrompt modWhatPromptBanner
  case choice of 'u' -> askPrompt "username"
                 'p' -> passwdPrompt
                 'e' -> askPrompt "email"
                 'a' -> askPrompt "admin"
                 'd' -> askPrompt "debugger"

replaceUsername (username, password, email, admin, debugger) newUsername = (newUsername, password, email, admin, debugger)
replacePassword (username, password, email, admin, debugger) newPassword = (username, newPassword, email, admin, debugger)
replaceEmail (username, password, email, admin, debugger) newEmail = (username, password, newEmail, admin, debugger)
replaceAdmin (username, password, email, admin, debugger) newAdmin = (username, password, email, newAdmin, debugger)
replaceDebugger (username, password, email, admin, debugger) newDebugger = (username, password, email, admin, newDebugger)



modWhatPromptBanner = unlines [ "u) username"
                              , "p) password"
                              , "e) email"
                              , "a) admin"
                              , "d) debugger"
                              , "q) quit" ]
-}
