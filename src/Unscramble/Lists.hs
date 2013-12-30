module Unscramble.Lists where

import System.Directory
import System.FilePath

lists :: [(String, String)]
lists = [("enable", "https://dotnetperls-controls.googlecode.com/files/enable1.txt")]

listPath :: String -> IO String
listPath s = fmap (</> (s ++ ".txt")) (getAppUserDataDirectory "unscramble")
