{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hgamelaunch.Prompts
import Hgamelaunch.BannerTools
import Hgamelaunch.DbTools
import Hgamelaunch.UserTools
import Hgamelaunch.GameTools
import System.Exit
import System.Directory
import Database.SQLite.Simple
import Data.Text (Text)

version :: Text
version = "0.1.0.0"

dbPath :: FilePath
dbPath = "./users.db"

main :: IO ()
main = do
  exists <- doesFileExist dbPath
  conn <- open dbPath
  makeDb conn exists
  menuLoop conn
  close conn

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
  banner <- getBanner "./config/banners/anonBanner.txt"
  c <- showBanner (editBanner banner)
  case c of 'l' -> login conn
            'r' -> register conn
            'w' -> watch conn
            'm' -> motd conn
            's' -> info conn
            'q' -> exitWith ExitSuccess
            _   -> anonMenu conn
  where editBanner = replaceVersion version

userMenu :: Connection -> Maybe UserField -> IO ()
userMenu conn (Just user) = do
  banner <- getBanner "./config/banners/userBanner.txt"
  games <- getGames
  c <- showBanner (editBanner banner user games)
  case c of 'c' -> changePassword conn user
            'e' -> changeEmail conn user
            'w' -> watchAsUser
            'q' -> exitWith ExitSuccess
            _   -> launchGame c user games
  userMenu conn (Just user)
  where editBanner b (UserField _ n _ _ _ _) g = replaceVersion version . replaceUser n . insertGames b $ showGames g
userMenu conn _ = menuLoop conn

adminMenu :: Connection -> Maybe UserField -> IO ()
adminMenu conn (Just user) = do
  banner <- getBanner "./config/banners/adminBanner.txt"
  c <- showBanner (editBanner user banner)
  case c of 'a' -> addUser conn
            'u' -> userMenu conn (Just user)
            'm' -> modUser conn
            'q' -> exitWith ExitSuccess
            _ -> adminMenu conn (Just user)
  adminMenu conn (Just user)
  where editBanner (UserField _ n _ _ _ _) = replaceVersion version . replaceUser n
adminMenu conn _ = menuLoop conn

addUser :: Connection -> IO ()
addUser conn = do
  userData <- addUserPrompt
  _ <- attemptRegister conn userData
  return ()

modUser :: Connection -> IO ()
modUser conn = do
  username <- askPrompt "Input the username of the user you want to modify"
  u <- case username of
    (Just username') -> fetchUserFromDb conn username'
    Nothing -> return Nothing

  user <- case u of
    (Just user') -> return user'
    Nothing -> error "User doesn't exist"

  banner <- getBanner "./config/banners/modUserBanner.txt"
  c <- showBanner (editBanner banner user)
  case c of 'u' -> modify conn user userUsername "username?"
            'p' -> modify conn user userPassword "password?"
            'e' -> modify conn user userEmail "email?"
            'a' -> modifyYn conn user userAdmin "admin?"
            'd' -> modifyYn conn user userDebugger "debug?"
            _ -> return ()
  where editBanner b (UserField _ n _ _ _ _) = replaceVersion version . replaceUser n $ b

-- PLACEHOLDER
watch :: Monad m => t -> m (Maybe UserField)
watch _ = return (Just (UserField 1 "codehero" "email" "password" True True))

motd :: Connection -> IO (Maybe UserField)
motd conn = do
  banner <- getBanner "./config/banners/motd.txt"
  _ <- showBanner banner
  anonMenu conn

info :: Connection -> IO (Maybe UserField)
info conn = do
  banner <- getBanner "./config/banners/serverInfo.txt"
  _ <- showBanner (replaceVersion version banner)
  anonMenu conn

watchAsUser :: IO ()
watchAsUser = return ()



{-
TODO:
- add mod(ify)User menu for admin
- template sharing/syncing
- selective game sharing (use symlinks for those)
-- store names of saves and with whom they are shared in extra table
-- or just store sharing status and make extra dir for shared saves
--- maybe have dir structure like:
---- /userfiles/username/sharedsaves/usernameofsharer/savefolders/... (user "guest" already knows which saves are shared with him, only has to look through folders)
---- sharer somehow has to keep track, this could also be solved by using files (saving a .sharedwith file in the appropriate save folder and reading it. there the client can just read out the usernames)
---- touch .isdebugger file when user wants to be debugger
-}
