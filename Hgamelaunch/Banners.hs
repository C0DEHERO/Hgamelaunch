module Hgamelaunch.Banners
       ( showBanner,
         anonBanner,
         userBanner,
         adminBanner
       ) where

import System.Console.ANSI

showBanner :: String -> IO Char
showBanner banner = do
  clearScreen
  putStrLn $ banner
  c <- getChar
  getChar
  return c

anonBanner :: String
anonBanner = unlines [ "DgameMonad v0.1"
                     , "l) Login"
                     , "r) Register new user"
                     , "w) Watch games in progress"
                     , "s) Server info"
                     , "m) MOTD/NEWS"
                     , "q) Quit" ]

userBanner :: String
userBanner = unlines [ "DgameMonad v0.1"
                     , "c) Change password"
                     , "e) Change email address"
                     , "p) Play game"
                     , "w) Watch games in progress"
                     , "q) Quit" ]

adminBanner :: String
adminBanner = unlines [ "DgameMonad v0.1"
                      , "a) Add user"
                      , "m) Modify user"
                      , "d) Delete user"
                      , "b) Ban user"
                      , "u) User menu"
                      , "q) Quit" ]
