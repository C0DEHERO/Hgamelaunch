{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Hgamelaunch.BannerTools
       ( showBanner,
         getBanner,
         replaceUser,
         replaceVersion,
         insertGames
       ) where

import System.Console.ANSI
import qualified Data.Text as T
import qualified Data.Text.IO as Ti

showBanner :: [T.Text] -> IO Char
showBanner banner = do
  clearScreen
  Ti.putStrLn $ T.unlines banner
  c <- getChar
  _ <- getChar
  return c

replaceVersion :: T.Text -> [T.Text] -> [T.Text]
replaceVersion version (x:xs) = (replaceVars version x) : replaceVersion version xs
  where replaceVars v = T.replace"%v" v
replaceVersion _ [] = []


replaceUser :: T.Text -> [T.Text] -> [T.Text]
replaceUser username (x:xs) = (replaceVars username x) : replaceUser username xs
  where replaceVars u = T.replace "%u" u
replaceUser _ [] = []


getBanner :: FilePath -> IO [T.Text]
getBanner path = do
  contents <- Ti.readFile path
  return $ T.lines contents

modUserBanner :: [String]
modUserBanner = [ "u) username"
                , "p) password"
                , "e) email"
                , "a) admin"
                , "d) debugger"
                , "q) quit" ]
                
insertGames :: [T.Text] -> [T.Text] -> [T.Text]
insertGames banner = joinThem (break ( == (T.pack "%games")) banner)
  where joinThem (x,y) g = x ++ g ++ drop 1 y
