import Data.List
import Data.Maybe
import Data.Ord
import Options.Applicative
import Paths_unscramble
import System.IO
import Unscramble.Input
import Unscramble.Output
import Unscramble.Score
import Unscramble.Search
import Unscramble.Types

parseOpts :: Parser ScrambleOpts
parseOpts = ScrambleOpts
        <$> option
            ( long "size" 
           <> short 's'
           <> metavar "SIZE"
           <> help "Grid size (default 4)"
           <> value 4)
        <*> option
            ( long "score"
           <> metavar "SYSTEM"
           <> help "Scoring system (one of \27[1mswf\27[0m (default),\
            \ \27[1mboggle\27[0m, \27[1mword-wars\27[0m)"
           <> value SWF)
        <*> option
            ( long "display"
           <> short 'd'
           <> metavar "STYLE"
           <> help "Display style: \27[1mone-line\27[0m to print all\
            \ the words in order on one line, or \27[1mchunked\27[0m\
            \ to display three-at-a-time with a board diagram for each."
           <> value Chunked)

main :: IO ()
main = do
    sopts <- execParser opts
    grid' <- readGrid (gridSize sopts)
    mult' <- case scoreSystem sopts of
        SWF -> readMult
        _ -> return $ Multiplier [] Nothing [] Nothing
    filepath <- getDataFileName "lists/enable.txt"
    withFile filepath ReadMode $ \h -> do
        cont <- hGetContents h
        let foundWords = sortBy ((invert .) . comparing (\(a,_,_) -> a))
                       . mapMaybe (search (grid',mult') (scoreSystem sopts))
                       $ lines cont
        display foundWords grid' mult' sopts
    where
        opts = info (helper <*> parseOpts)
            ( fullDesc
           <> progDesc "Solve a Boggle-like word game"
           <> header "unscramble")
        invert LT = GT
        invert GT = LT
        invert x = x
