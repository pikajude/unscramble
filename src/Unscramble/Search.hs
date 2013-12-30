{-# LANGUAGE TupleSections #-}

module Unscramble.Search (search) where

import Control.Arrow
import Control.Monad
import qualified Data.HashMap.Strict as H
import Data.List.Stream
import Data.Maybe
import Data.Ord
import Prelude hiding (map, filter, reverse, head, concatMap)
import Unscramble.Score
import Unscramble.Types

tokenize :: String -> [String]
tokenize = groupBy (\x y -> [x,y] == "qu")

search :: Search
       -> ScoringSystem
       -> String
       -> Maybe (Score, String, [Coordinate])

search s@(grid,_) system word = putWordIn . reversePath . chooseBestPath $
                                  walk firstPaths tokens
  where putWordIn             = fmap $ \(a, b) -> (a, word, b)
        reversePath           = fmap (second reverse)
        (firstToken : tokens) = tokenize word
        lets                  = letters grid
        firstTokenPositions   = H.lookupDefault [] firstToken lets
        firstPaths            = map (: []) firstTokenPositions

        chooseBestPath :: [[Coordinate]] -> Maybe (Score, [Coordinate])

        chooseBestPath paths =
          listToMaybe . sortBy (comparing fst) $
            map (\path -> (score system s path, path)) paths

        -- new and improved algorithm written by Devyn Cairns
        -- <devyn.cairns@gmail.com>
        -- thanks buddy
        walk :: [[Coordinate]] -> [String] -> [[Coordinate]]

        walk [] _     = []
        walk paths [] = paths
        walk paths (token : remaining) =
          let tokenPositions = H.lookupDefault [] token lets
              branch path =
                map (: path) $
                  filter (isNeighbor (head path)) (tokenPositions \\ path)
              paths' = concatMap branch paths
          in  walk paths' remaining

isNeighbor :: Coordinate -> Coordinate -> Bool
isNeighbor (x,y) (a,b)
  | (x,y) == (a,b) = False
  | otherwise      = let xa = x - a
                         yb = y - b
                     in  (xa >= -1 && xa <= 1) && (yb >= -1 && yb <= 1)
