{-# LANGUAGE TupleSections #-}

module Unscramble.Search (search) where

import Control.Monad
import qualified Data.HashMap.Strict as H
import Data.List.Stream
import Data.Maybe
import Data.Ord
import Prelude hiding ((++), filter, foldr, head, notElem, null, map, concat, concatMap, length, lines, splitAt, zip, reverse)
import Unscramble.Score
import Unscramble.Types

tokenize :: String -> [String]
tokenize = groupBy (\x y -> [x,y] == "qu")

search :: Search -> ScoringSystem -> String -> Maybe (Score,String,[Coordinate])
search sy@(grid,_) ss word = putWordIn $ walk firstPaths tokens
  where (firstToken : tokens) = tokenize word
        putWordIn             = fmap $ \(a,b) -> (a,word,b)
        lets                  = letters grid
        firstTokenPositions   = H.lookupDefault [] firstToken lets
        firstPaths            = map (: []) firstTokenPositions

        -- new and improved algorithm written by devyn cairns (devyn.me)
        -- thanks buddy
        walk :: [[Coordinate]] -> [String] -> Maybe (Score,[Coordinate])

        walk [] _     = Nothing
        walk paths [] = listToMaybe . sortBy (comparing fst)
                      $ map (\a -> (score ss sy a,a)) paths
        walk paths (token : remaining) =
          let tokenPositions = H.lookupDefault [] token lets
              branch path =
                map (: path) $
                  filter (isNeighbor (head path)) (tokenPositions \\ path)
              paths' = concatMap branch paths
          in  walk paths' remaining
        {-# INLINE walk #-}

isNeighbor :: (Int,Int) -> (Int,Int) -> Bool
isNeighbor (x,y) (a,b)
  | (x,y) == (a,b) = False
  | otherwise      = let xa = x - a
                         yb = y - b
                     in  (xa >= -1 && xa <= 1) && (yb >= -1 && yb <= 1)
{-# INLINE isNeighbor #-}
