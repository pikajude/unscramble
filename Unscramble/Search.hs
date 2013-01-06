module Unscramble.Search (
    searchGrid
) where

import Control.Applicative
import Control.Lens
import Control.Monad.RWS
import qualified Data.HashMap as H
import Data.List
import Data.Ord
import Unscramble.Score
import Unscramble.Types

searchGrid :: String
           -> Grid
           -> Multiplier
           -> ScoringSystem
           -> Maybe (Score, String, [Coordinate])
searchGrid s g m sys = fmap (\(a,b) -> (a,s,b)) . fst
                     $ evalRWS (search (tokenize s)) (g, m, sys) ([],[])

tokenize :: String -> [String]
tokenize = groupBy (\x y -> [x,y] == "qu")

isNeighbor :: Coordinate -> Coordinate -> Bool
isNeighbor (x1,y1) (x2,y2) = deltax <= 1 && deltay <= 1 && deltax + deltay > 0
    where
        deltax = abs $ x2 - x1
        deltay = abs $ y2 - y1

search :: [String] -> Search (Maybe (Score, [Coordinate]))
search (x:xs) = do
    startCoords <- use start
    l <- view (grid.letters)
    let valid' = filter (\y -> null startCoords || any (isNeighbor y) startCoords)
               $ H.findWithDefault [] x l
    if null valid'
        then return Nothing
        else do
            start .= valid' -- set the new starting coordinates to valid ones
            valid %= (valid':) -- add the starting coordinates to the list
            search xs
search [] = validate

validate :: Search (Maybe (Score, [Coordinate]))
validate = do
    xs <- use valid
    scoreSys <- view ss
    let validSequences = map reverse
                       . filter (\n -> n == nub n
                                    && and (zipWith isNeighbor `ap` tail $ n))
                       $ sequence xs
    scords <- mapM (\c -> (,c) <$> score scoreSys c) validSequences
    case reverse $ sortBy (comparing fst) scords of
        [] -> return Nothing
        ((a,b):_) -> return $ Just (a, b)