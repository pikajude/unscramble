module Unscramble.Output (
    display
) where

import Control.Lens
import qualified Data.HashMap as H
import Data.List
import Data.Ord
import Text.Printf
import Unscramble.Types

-- words are already sorted by score
display :: [(Score, String, [Coordinate])]
        -> Grid
        -> Multiplier
        -> ScrambleOpts
        -> IO ()
display xs g m (ScrambleOpts _ _ ds)
     | ds == OneLine = (showTotal >>) $ mapM_ (\x -> putStr ((_2 ^$ x) ++ " ")) xs
     | ds == Chunked = (showTotal >>) $ mapM_ (\ns -> do
         mapM_ (displayGrid g m) ns
         putStrLn "\nPress enter for more, or ctrl-c to quit."
         getLine) $ chunk xs
    where
        showTotal = do
            let points = sum . map (_1 ^$) $ xs
            printf "%d total words for %d points.\n" (length xs) points
        chunk (a:b:c:es) = [a,b,c]:chunk es
        chunk x = [x]
display _ _ _ (ScrambleOpts _ _ _) = error "what?"

displayGrid :: Grid
            -> Multiplier
            -> (Score, String, [Coordinate])
            -> IO ()
displayGrid (Grid _ gs) (Multiplier dl dw tl tw) (sc, word, cs)
        = (putStrLn ("\n\27[1m" ++ word ++ "\27[0m: " ++ show sc) >>)
        . mapM_ (putStr . render) $ idxs
    where
        max' = fst . maximumBy (comparing fst) $ H.keys gs
        idxs = [(a,b) | a <- [0..max'], b <- [0..max']]
        render n
            | head cs == n = "\27[44m" ++ pad letter n True ++ "\27[49m" ++ nl
            | n `elem` cs = "\27[107m" ++ pad letter n True ++ "\27[49m" ++ nl
            | otherwise = pad letter n False ++ nl
            where
                letter = H.findWithDefault undefined n gs
                nl = if snd n == max' then "\n" else ""
        pad [x] n t = " " ++ highlight [x] n t ++ " "
        pad x n t = ' ':highlight x n t
        highlight x n t
            | n `elem` dl = "\27[36;1m" ++ x ++ "\27[39;22m"
            | Just n == dw = "\27[31;1m" ++ x ++ "\27[39;22m"
            | n `elem` tl = "\27[32;1m" ++ x ++ "\27[39;22m"
            | Just n == tw = "\27[33;1m" ++ x ++ "\27[39;22m"
            | t = "\27[30m" ++ x ++ "\27[39;22m"
            | otherwise = x