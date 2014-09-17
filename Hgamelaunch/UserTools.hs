module Hgamelaunch.UserTools
       ( attemptLogin,
         attemptRegister,
         registerNormal,
         updatePassword,
         updateEmail
         ) where

import Hgamelaunch.DbTools
import Database.SQLite.Simple(Connection)
import Data.Text(pack, unpack)

attemptLogin :: Connection -> (String, String) -> IO (Maybe UserField)
attemptLogin conn (username, pass) = do
  user <- fetchUserFromDb conn username
  case user of
    (Just (UserField _ _ password _ _ _)) -> if pass == unpack password then return user else return Nothing
    Nothing -> return Nothing


attemptRegister :: Connection -> Maybe (String,String,String,Bool,Bool) -> IO (Maybe UserField)
attemptRegister conn (Just (username, pass, email, admin, debugger)) = do
  user <- fetchUserFromDb conn username :: IO (Maybe UserField)
  case user of
    Nothing -> do
      insertUser conn username pass email admin debugger
      fetchUserFromDb conn username
    (Just _) -> return Nothing
attemptRegister _ Nothing = return Nothing


registerNormal :: Connection -> Maybe (String, String, String) -> IO (Maybe UserField)
registerNormal conn (Just (username, pass, email)) = attemptRegister conn (Just (username, pass, email, False, False))
registerNormal _ Nothing = return Nothing


updatePassword :: Connection -> UserField -> Maybe String -> IO ()
updatePassword conn user@(UserField _ username _ email admin debugger) (Just password) = updateUser conn user username (pack password) email admin debugger
updatePassword _ _ Nothing = return ()


updateEmail :: Connection -> UserField -> Maybe String -> IO ()
updateEmail conn user@(UserField _ username password _ admin debugger) (Just email) = updateUser conn user username password (pack email) admin debugger
updateEmail _ _ Nothing = return ()
