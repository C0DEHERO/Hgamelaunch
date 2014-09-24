{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Hgamelaunch.GameTools
       ( getGames,
         showGames,
         launchGame,
         rootPath,
         userDir,
         inprogressDir,
         ttyrecDir,
         templateCfg,
         cfgFile,
         Game(Game)
       ) where

import Hgamelaunch.DbTools
import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad (mzero)
import qualified Data.ByteString.Lazy as B
import System.Process

data Game = Game
      { choiceCh :: Text
      , shortName :: Text
      , gameName :: Text
      , rootPath :: Text
      , gamePath :: Text
      , userDir :: Text
      , inprogressDir :: Text
      , ttyrecDir :: Text
      , templateCfg :: Text
      , cfgFile :: Text
      , gameArgs :: [Text]
      } deriving Show

instance FromJSON Game where
  parseJSON (Object v) = Game <$>
                         v .: "choice" <*>
                         v .: "shortname" <*>
                         v .: "gamename" <*>
                         v .: "rootpath" <*>
                         v .: "gamepath" <*>
                         v .: "userdir" <*>
                         v .: "inprogress" <*>
                         v .: "ttyrecdir" <*>
                         v .: "templatecfg" <*>
                         v .: "cfgfile" <*>
                         v .: "gameargs"
  parseJSON _           = mzero

getGames :: IO [Game]
getGames = do
  contents <- B.readFile "./config/games.json"
  return (checkGames (decode contents :: Maybe [Game]))
    where checkGames (Just games) = games
          checkGames Nothing = error "Could not read games.json!"

showGames :: [Game] -> [Text]
showGames ((Game {choiceCh = choice, shortName = name}):xs) = (choice `append` ") " `append` name) : showGames xs
showGames [] = []

launchGame :: Char -> UserField -> [Game] -> IO ()
launchGame c user (game@(Game choice _ _ rootpath gamepath userdir _ _ _ _ _):xs)
  | (cons c "") == choice = do
      callProcess (unpack (replace "%r" rootpath gamepath)) (makeArgs user game)
  | otherwise = launchGame c user xs
        where makeArgs u@(UserField _ username _ _ _ _) (Game {gameArgs = args}) = replaceInArgs args rootpath userdir username ++
                                                                                   addPrivs u
launchGame _ _ _ = return ()

replaceInArgs :: [Text] -> Text -> Text -> Text -> [String]
replaceInArgs (x:xs) rootpath userdir username =
  unpack (replaceVars  x) : replaceInArgs xs rootpath userdir username
  where replaceVars = replace "%r" rootpath . replace "%u" (replaceInUserdir userdir) . replace "%n" username
        replaceInUserdir = replace "%r" rootpath . replace "%n" username
replaceInArgs [] _ _ _ = []


addPrivs :: UserField -> [String]
addPrivs user = addAdminArg user ++ addDebuggerArg user
                where addAdminArg (UserField _ username _ _ admin _) = if admin then ["--addadmin", unpack username] else []
                      addDebuggerArg (UserField _ username _ _ _ debugger) = if debugger then ["--adddebugger", unpack username] else []
