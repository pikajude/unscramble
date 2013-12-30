module Unscramble.Input (
    readGrid,
    readMult
) where

import Control.Applicative
import Control.Monad
import Data.Array
import Data.Char
import qualified Data.HashMap.Strict as H
import Data.List
import Data.Maybe
import System.IO
import Text.Printf
import Unscramble.Types

readMult :: IO Multiplier
readMult = do
    dls <- putStrLn "Enter coordinates for double letters, in\
           \ the form 'x y'. The lower left corner is '1 1'\
           \ and the top right is '4 4'. Press enter after each\
           \ set of coordinates. If there are no double\
           \ letters, press enter." >> getCoords
    dws <- putStrLn "Enter coordinates for double words. If there\
           \ are no double words, press enter." >> getCoords
    tls <- putStrLn "Enter coordinates for triple letters. If there\
           \ are no triple letters, press enter." >> getCoords
    tws <- putStrLn "Enter coordinates for triple words. If there\
           \ are no triple words, press enter." >> getCoords
    return $ Multiplier dls (listToMaybe dws) tls (listToMaybe tws)
    where
        getCoords = do
            eo <- isEOF
            if eo
                then return []
                else do
                    r <- getLine
                    case words r of
                        [a,b] -> do
                            l <- getCoords
                            return $ (4 - read b, read a - 1):l
                        [] -> return []
                        _ -> putStrLn "Invalid coordinates." >> return []

readGrid :: Int -> IO Grid
readGrid n = do
    _ <- printf "Enter a %dx%d grid. Remember:\n\
        \- Input only letters and press enter after each line.\n\
        \- You may enter a 'qu' tile as 'qu'.\n" n n
    lines' <- replicateM n (filter isLetter <$> getLine)
    putStrLn "Received grid."
    return $ arraign lines'

arraign :: [String] -> Grid
arraign grid = Grid (hash ar) ar
  where ar = array ((0,0),(len,len)) . redist . zip [0..]
           $ map (zip [0..] . groupBy (\x y -> [x,y] == "qu")) grid
        hash a = foldr (\(i,k) h -> H.insertWith (flip (++)) k [i] h) H.empty (assocs a)
        len = pred . length $ head grid
        redist ((x,ys):xs) = map (\(a,b) -> ((a,x),b)) ys ++ redist xs
        redist [] = []
