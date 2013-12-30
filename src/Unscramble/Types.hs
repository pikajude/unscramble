module Unscramble.Types (
    ScrambleOpts(..),

    Coordinate,
    Multiplier(..),
    Grid(..),
    Search,

    ScoringSystem(..),
    Score,

    DisplayStyle(..)
) where

import Data.Array
import Data.Char
import qualified Data.HashMap.Strict as H
import Data.List

type Tile = String

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

data Grid = Grid { letters :: H.HashMap Tile [Coordinate]
                 , coords :: Array (Int,Int) Tile
                 }

type Search = (Grid, Multiplier)
