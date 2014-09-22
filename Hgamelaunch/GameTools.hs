{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Hgamelaunch.GameTools
       ( getGames,
         showGames,
         launchGame
       ) where

import Hgamelaunch.DbTools
import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad (mzero)
import qualified Data.ByteString.Lazy as B
import System.Process

data Game = Game
      { choicechar :: Text
      , shortName :: Text
      , gameName :: Text
      , rootPath :: Text
      , gamePath :: Text
      , gameArgs :: [Text]
      } deriving Show

instance FromJSON Game where
  parseJSON (Object v) = Game <$>
                         v .: "choice" <*>
                         v .: "shortname" <*>
                         v .: "gamename" <*>
                         v .: "rootpath" <*>
                         v .: "gamepath" <*>
                         v .: "gameargs"
  parseJSON _           = mzero

getGames :: IO [Game]
getGames = do
  contents <- B.readFile "./config/games.json"
  return (checkGames (decode contents :: Maybe [Game]))
    where checkGames (Just games) = games
          checkGames Nothing = error "Could not read games.json!"

showGames :: [Game] -> [Text]
showGames ((Game choice name _ _ _ _):xs) = (choice `append` ") " `append` name) : showGames xs
showGames [] = []

launchGame :: Char -> UserField -> [Game] -> IO ()
launchGame c user (game@(Game choice _ _ rootpath gamepath _):xs)
  | (cons c "") == choice = do
      callProcess (unpack (replace "%r" rootpath gamepath)) (makeArgs user game)
  | otherwise = launchGame c user xs
        where makeArgs u@(UserField _ username _ _ _ _) (Game _ _ _ _ _ args) = replaceInArgs args rootpath username ++ addPrivs u
launchGame _ _ _ = return ()

replaceInArgs :: [Text] -> Text -> Text -> [String]
replaceInArgs (x:xs) rootpath username = (unpack (replaceVars rootpath username x)) : replaceInArgs xs rootpath username
                                       where replaceVars r u = replace "%r" r . replace "%u" u
replaceInArgs [] _ _ = []


addPrivs :: UserField -> [String]
addPrivs user = addAdminArg user ++ addDebuggerArg user
                where addAdminArg (UserField _ username _ _ admin _) = if admin then ["--addadmin", unpack username] else []
                      addDebuggerArg (UserField _ username _ _ _ debugger) = if debugger then ["--adddebugger", unpack username] else []
