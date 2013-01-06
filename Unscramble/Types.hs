module Unscramble.Types (
    ScrambleOpts(..),
    
    Coordinate,
    Multiplier(..),
    Grid(..),
    letters,
    coords,
    
    grid,
    mult,
    ss,
    start,
    valid,
    
    ScoringSystem(..),
    Score,
    
    DisplayStyle(..),
    
    Search
) where

import Control.Lens
import Control.Monad.RWS
import Data.Char
import qualified Data.HashMap as H
import Data.List

data ScrambleOpts = ScrambleOpts { gridSize :: Int
                                 , scoreSystem :: ScoringSystem
                                 , displayStyle :: DisplayStyle
                                 }

data ScoringSystem = Boggle | SWF | WordWars

data DisplayStyle = Chunked | OneLine deriving (Eq)

type Score = Int

instance Read ScoringSystem where
    readsPrec _ x
        | "boggle" `isPrefixOf` y = wrap Boggle $ drop 6 y
        | "swf" `isPrefixOf` y = wrap SWF $ drop 3 y
        | "scramble" `isPrefixOf` y = wrap SWF $ drop 8 y
        | "wordwars" `isPrefixOf` y = wrap WordWars $ drop 8 y
        | "word-wars" `isPrefixOf` y = wrap WordWars $ drop 9 y
        | otherwise = []
        where
            y = map toLower x
            wrap= (return .) . (,)

instance Read DisplayStyle where
    readsPrec _ x
        | "chunked" `isPrefixOf` y = wrap Chunked $ drop 7 y
        | "one-line" `isPrefixOf` y = wrap OneLine $ drop 8 y
        | otherwise = []
        where
            y = map toLower x
            wrap = (return .) . (,)

type Coordinate = (Int,Int)

data Multiplier = Multiplier { doubleLetter :: [Coordinate]
                             , doubleWord   :: Maybe Coordinate
                             , tripleLetter :: [Coordinate]
                             , tripleWord   :: Maybe Coordinate
                             }

data Grid = Grid { _letters :: H.Map String [Coordinate]
                 , _coords :: H.Map Coordinate String
                 }
                 
makeLenses ''Grid

type Search = RWS (Grid, Multiplier, ScoringSystem) () ([Coordinate], [[Coordinate]])

grid :: Simple Lens (Grid, Multiplier, ScoringSystem) Grid
grid = _1
mult :: Simple Lens (Grid, Multiplier, ScoringSystem) Multiplier
mult = _2
ss :: Simple Lens (Grid, Multiplier, ScoringSystem) (ScoringSystem)
ss = _3
start :: Simple Lens ([Coordinate], [[Coordinate]]) [Coordinate]
start = _1
valid :: Simple Lens ([Coordinate], [[Coordinate]]) [[Coordinate]]
valid = _2