{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Hgamelaunch.UserTools
       ( login,
         register,
         modify,
         modifyYn,
         attemptLogin,
         attemptRegister,
         registerRegular,
         changePassword,
         changeEmail
         ) where

import Hgamelaunch.DbTools
import Hgamelaunch.GameTools
import Hgamelaunch.Prompts
import Database.SQLite.Simple(Connection)
import Data.Text(Text, pack, unpack, replace)
import Control.Lens
import System.Process
import System.Directory

login :: Connection -> IO (Maybe UserField)
login conn = do
  putStrLn "Input empty line to abort"
  userData <- loginPrompt
  attemptLogin conn userData
  
register :: Connection -> IO (Maybe UserField)
register conn = do
  putStrLn "Input empty line to abort"
  userData <- registerPrompt
  registerRegular conn userData

modify conn user lens question = do
  result <- askPrompt question
  case result of
   (Just value) -> updateUser conn (set lens value user)
   Nothing -> return ()

modifyYn conn user lens question = do
  result <- ynPrompt question
  updateUser conn (set lens result user)

attemptLogin :: Connection -> Maybe (Text, Text) -> IO (Maybe UserField)
attemptLogin conn (Just (username, pass)) = do
  user <- fetchUserFromDb conn username
  case user of
    (Just (UserField _ _ password _ _ _)) -> if pass == password then return user else return Nothing
    Nothing -> return Nothing
attemptLogin _ Nothing = return Nothing

attemptRegister :: Connection -> Maybe UserField -> IO (Maybe UserField)
attemptRegister conn (Just user@(UserField _ username _ _ _ _)) = do
  result <- fetchUserFromDb conn username :: IO (Maybe UserField)
  case result of
    Nothing -> do
      games <- getGames
      createUserDirs username games
      insertUser conn user
      fetchUserFromDb conn username
    (Just _) -> return Nothing
attemptRegister _ Nothing = return Nothing

registerRegular :: Connection -> Maybe (Text, Text, Text) -> IO (Maybe UserField)
registerRegular conn (Just (username, pass, email)) = attemptRegister conn (Just (UserField 0 username pass email False False))
registerRegular _ Nothing = return Nothing

changePassword :: Connection -> UserField -> IO ()
changePassword conn user = do
  putStrLn "Input empty line to abort"
  result <- passwdPrompt "your new password"
  updateUser conn (setMaybeUserLens user userPassword result)

changeEmail :: Connection -> UserField -> IO ()
changeEmail conn user = do
  putStrLn "Input empty line to abort"
  result <- emailPrompt
  updateUser conn (setMaybeUserLens user userEmail result)

setMaybeUserLens :: forall t a b.t -> ASetter t t a b -> Maybe b -> t
setMaybeUserLens u l (Just value) = set l value u
setMaybeUserLens u _ Nothing = u

createUserDirs :: Text ->  [Game] -> IO()
createUserDirs username ((Game {..}):games) = do
  createDirectoryIfMissing True (substitute userDir)
  createDirectoryIfMissing True (substitute inprogressDir)
  createDirectoryIfMissing True (substitute ttyrecDir)
  templateExists <- doesFileExist (substitute templateCfg)
  cpTemplate (substitute templateCfg) (substitute cfgFile) templateExists
  createUserDirs username games
    where substitute old = unpack $ replace "%n" username . replace "%r" rootPath . replace "%u" userDir $ old
          cpTemplate old new True = copyFile old new
          cpTemplate _ _ False = return ()
createUserDirs _ [] = return ()
