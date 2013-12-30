module Unscramble.Download (
  fetch
) where

import           Control.Monad
import qualified Data.Conduit as C
import           Data.Conduit.Binary (sinkFile)
import           Network.HTTP.Conduit
import           System.Directory
import           System.FilePath
import           System.IO
import           Unscramble.Lists

fetch :: IO ()
fetch = do
    dir <- getAppUserDataDirectory "unscramble"
    createDirectoryIfMissing True dir
    putStr "Checking lists..."
    hFlush stdout
    forM_ lists (fetchList dir)
    putStrLn "done."
    hFlush stdout
    where
        fetchList dir (name,url) = do
            let fullPath = dir </> (name ++ ".txt")
            fy <- doesFileExist fullPath
            unless fy $ do
                putStr $ "Fetching " ++ name ++ ".txt..."
                hFlush stdout
                request <- parseUrl url
                withManager $ \man -> do
                    response <- http request man
                    responseBody response C.$$+- sinkFile fullPath
                putStrLn "done."
                hFlush stdout
