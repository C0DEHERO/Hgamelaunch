{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hgamelaunch.Banners
import Hgamelaunch.Prompts
import Hgamelaunch.DbTools
import Hgamelaunch.UserTools
import Data.Text (unpack)
--import System.Console.ANSI
import System.Exit
import System.Process
----------------------------
--import Control.Applicative
--import Control.Monad
import Database.SQLite.Simple
--import Database.SQLite.Simple.FromRow

{-
anonActions = [ ('l', login), ('r', register), ('w', watch), ('s', info), ('m', motd), ('q', quit) ]
userActions = [ ('c', passwd), ('e', email), ('p', play), ('w', watch), ('q', quit) ]
adminActions = [ ('a', adduser), ('d', deluser), ('b', banuser), ('q', quit) ]
-}

main :: IO ()
main = do
  conn <- open ":memory:"
  makeDb conn
  menuLoop conn
  close conn
--  menuLoop
--  menuResult <- anonMenu
--  nextMenu menuResult


{-
fetchPassFromDb :: Connection -> String -> IO String
fetchPassFromDb conn username = do
  user <- fetchUserFromDb conn username :: IO (Maybe UserField)
  return (getUserPassword user)
  
-- getUserPassword :: (FromRow r) => [r] -> String
getUserPassword :: Maybe UserField -> String
getUserPassword (Just (UserField _ _ password _ _ _)) = unpack password
getUserPassword Nothing = ""
-}

printUser :: Maybe String -> IO ()
printUser (Just user) = print user
printUser Nothing = print ("User doesn't exist" :: String)

menuLoop :: Connection -> IO()
menuLoop conn = do
  menuResult <- anonMenu conn
  nextMenu conn menuResult

nextMenu :: Connection -> Maybe UserField -> IO ()
nextMenu conn Nothing = menuLoop conn
nextMenu conn user@(Just (UserField _ _ _ _ True _)) = adminMenu conn user
nextMenu conn user = userMenu conn user


anonMenu :: Connection -> IO (Maybe UserField)
anonMenu conn = do
  c <- showBanner anonBanner
  case c of 'l' -> login conn
            'r' -> register conn
            'w' -> watch conn
            'q' -> exitWith ExitSuccess
            _   -> anonMenu conn

userMenu :: Connection -> Maybe UserField -> IO ()
userMenu conn (Just user) = do
  c <- showBanner userBanner
  case c of 'c' -> changePassword conn user
            'e' -> changeEmail conn user
            'p' -> play user
            'w' -> watchAsUser
            'q' -> exitWith ExitSuccess
            _   -> userMenu conn (Just user)
userMenu conn _ = menuLoop conn

adminMenu :: Connection -> Maybe UserField -> IO ()
adminMenu conn (Just user) = do
  c <- showBanner adminBanner
  case c of 'a' -> addUser conn
            'u' -> userMenu conn (Just user)
            _ -> adminMenu conn (Just user)
--            'm' -> modUser conn
adminMenu conn _ = menuLoop conn

addUser :: Connection -> IO ()
addUser conn = do
  userData <- addUserPrompt
  _ <- attemptRegister conn userData
  return ()

{-
modUser :: Connection -> IO ()
modUser conn = do
  userData <- modUserPrompt
-}

login :: Connection -> IO (Maybe UserField)
login conn = do
  userData <- loginPrompt
  attemptLogin conn userData
  
register :: Connection -> IO (Maybe UserField)
register conn = do
  userData <- registerPrompt
  registerNormal conn userData

-- PLACEHOLDER
watch :: Monad m => t -> m (Maybe UserField)
watch _ = return (Just (UserField 1 "codehero" "email" "password" True True))

watchAsUser :: IO ()
watchAsUser = return ()

changePassword :: Connection -> UserField -> IO ()
changePassword conn user = do
  result <- passwdPrompt
  updatePassword conn user result

changeEmail :: Connection -> UserField -> IO ()
changeEmail conn user = do
  result <- emailPrompt
  updateEmail conn user result

play :: UserField -> IO ()
play user = do
  callProcess gamepath (makeArgs user)
  return ()
    where
      gamepath = "./testingenv/cdda/cataclysm"

makeArgs :: UserField -> [String]
makeArgs u@(UserField _ username _ _ _ _) = ["--username", unpack username,
                                             "--basepath", rootpath,
                                             "--userdir", userpath username,
                                             "--savedir", savepath,
                                             "--memorial", memorialpath,
                                             "--shared"] ++ addAdminDebuggerArg u
  where addAdminDebuggerArg user' = addAdminArg user' ++ addDebuggerArg user'
        rootpath = "./testingenv/"
        userpath username' = (rootpath ++ "userdata/" ++ unpack username' ++ "/")
        sharepath = (rootpath ++ "share/")
        savepath = (sharepath ++ "save/")
        memorialpath = (sharepath ++ "memorial/")

addAdminArg :: UserField -> [String]
addAdminArg (UserField _ username _ _ admin _) = if admin then ["--addadmin", unpack username] else []
addDebuggerArg :: UserField -> [String]
addDebuggerArg (UserField _ username _ _ _ debugger) = if debugger then ["--adddebugger", unpack username] else []

{-
TODO:
- template sharing/syncing
- selective game sharing (use symlinks for those)
-- store names of saves and with whom they are shared in extra table
-- or just store sharing status and make extra dir for shared saves
--- maybe have dir structure like:
---- /userfiles/username/sharedsaves/usernameofsharer/savefolders/... (user "guest" already knows which saves are shared with him, only has to look through folders)
---- sharer somehow has to keep track, this could also be solved by using files (saving a .sharedwith file in the appropriate save folder and reading it. there the client can just read out the usernames)
---- touch .isdebugger file when user wants to be debugger
-}
