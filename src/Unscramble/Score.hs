module Unscramble.Score (
    ScoringSystem(..),
    score
) where

import Data.Array ((!))
import Data.Maybe
import Unscramble.Types

score :: ScoringSystem -> Search -> [Coordinate] -> Int
score Boggle _ = scoreBoggle
score SWF s = scoreSWF s
score WordWars s = scoreWordWars s

scoreBoggle :: [Coordinate] -> Int
scoreBoggle xs = case length xs of
    3 -> 1
    4 -> 1
    5 -> 2
    6 -> 3
    7 -> 5
    x | x >= 8 -> 11
    _ -> 0

scoreWordWars :: Search -> [Coordinate] -> Int
scoreWordWars (Grid _ cs,_) xs = sum . map (\x -> fromMaybe 0 $ lookup x scores) $ lets
    where
        lets = map (cs !) xs
        scores = [ ("a", 1), ("b", 4), ("c", 3), ("d", 2)
                 , ("e", 1), ("f", 2), ("g", 3), ("h", 3)
                 , ("i", 1), ("j", 6), ("k", 5), ("l", 2)
                 , ("m", 4), ("n", 2), ("o", 1), ("p", 4)
                 , ("qu", 8), ("r", 1), ("s", 1), ("t", 1)
                 , ("u", 3), ("v", 4), ("w", 4), ("x", 8)
                 , ("y", 2), ("z", 8) ]

scoreSWF :: Search -> [Coordinate] -> Int
scoreSWF (Grid _ gs, Multiplier dl dw tl tw) cs = if length cs == 2
    then 1
    else let dwm = case dw of
                      Nothing -> id
                      Just r -> if r `elem` cs then (*2) else id
             twm = case tw of
                       Nothing -> id
                       Just r -> if r `elem` cs then (*3) else id
             scoreOf n = dlm . tlm $ baseScore n
                 where
                     baseScore q = fromJust $ lookup (gs ! q) scores
                     dlm = if n `elem` dl then (*2) else id
                     tlm = if n `elem` tl then (*3) else id

         in (+ lengthBonus (length cs)) . dwm . twm . sum
               $ map scoreOf cs
    where
        scores = [ ("a", 1), ("b", 4), ("c", 4), ("d", 2)
                 , ("e", 1), ("f", 4), ("g", 3), ("h", 3)
                 , ("i", 1), ("j", 10), ("k", 5), ("l", 2)
                 , ("m", 4), ("n", 2), ("o", 1), ("p", 4)
                 , ("qu", 10), ("r", 1), ("s", 1), ("t", 1)
                 , ("u", 2), ("v", 5), ("w", 4), ("x", 8)
                 , ("y", 3), ("z", 10) ]
        lengthBonus 5 = 3
        lengthBonus 6 = 6
        lengthBonus 7 = 10
        lengthBonus 8 = 15
        lengthBonus 9 = 20
        lengthBonus x | x >= 10 = 25
        lengthBonus _ = 0
