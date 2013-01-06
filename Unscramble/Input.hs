module Unscramble.Input (
    readGrid,
    readMult
) where

import Control.Applicative
import Control.Arrow
import Control.Lens
import Data.Char
import qualified Data.HashMap as H
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
    lines' <- sequence $ replicate n (filter isLetter <$> getLine)
    putStrLn "Received grid."
    return $ parseGrid lines'

parseGrid :: [String] -> Grid
parseGrid input = uncurry Grid
                . (letterMap &&& coordMap)
                . itoListOf (ifolded <.> ifolded)
                $ map tokenize input
    where
        letterMap [] = H.empty
        letterMap (((x,y),str):xs) = H.insertWith (++) str [(x,y)] $! letterMap xs
        coordMap = foldr (uncurry H.insert) H.empty
        tokenize = groupBy (\x y -> [x,y] == "qu")