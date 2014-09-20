{-# LANGUAGE RankNTypes #-}
module Hgamelaunch.UserTools
       ( attemptLogin,
         attemptRegister,
         registerNormal,
         setMaybeUserLens
         ) where

import Hgamelaunch.DbTools
import Database.SQLite.Simple(Connection)
import Data.Text(Text, pack, unpack)
import Control.Lens

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
      insertUser conn user
      fetchUserFromDb conn username
    (Just _) -> return Nothing
attemptRegister _ Nothing = return Nothing


registerNormal :: Connection -> Maybe (Text, Text, Text) -> IO (Maybe UserField)
registerNormal conn (Just (username, pass, email)) = attemptRegister conn (Just (UserField 0 username pass email False False))
registerNormal _ Nothing = return Nothing

setMaybeUserLens :: forall t a b.t -> ASetter t t a b -> Maybe b -> t
setMaybeUserLens u l (Just value) = set l value u
setMaybeUserLens u _ Nothing = u
