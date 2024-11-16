module RenderOpts where

import Data.Maybe 
import Options.Applicative
import Text.Read (readEither)

data RenderOpts = RenderOpts {
    movesPerDiagram :: Int,
    diagramsPerPage :: Int,
    input ::  FilePath,
    output :: FilePath,
    host:: String,
    scoreEstimate :: Bool
} deriving (Show)

-- Define the custom parser
validateDiagramsPerPage :: Int -> Either String Int
validateDiagramsPerPage val 
      | val >0 && val <=4 = Right val
      | otherwise = Left "diagramsPerPage must be between 1 and 4."

parseDiagramsPerPage :: String -> Either String Int
parseDiagramsPerPage dpp = do i <- readEither dpp    
                              validateDiagramsPerPage i

parseOpts :: Parser RenderOpts
parseOpts = RenderOpts <$>
  option auto (
       long "movesPerDiagram"
          <> metavar "movesPerDiagram"
          <> showDefault
          <> value 5
          <> help "how many moves to show in each diagram")
    <*> option (eitherReader parseDiagramsPerPage) 
       (long "diagramsPerPage"
          <> metavar "diagramsPerPage"
          <> showDefault
          <> value 2
          <> help "how many diagrams to show on each page")
    <*> strOption ( long "input" 
          <> short 'i'
          <> metavar "INPUT" 
          <> help "input file path")
    <*> strOption ( long "output" 
        <> short 'o'
        <> metavar "OUTPUT" 
        <> help "output file pattern")
    <*> strOption ( long "host" 
        <> short 'h'
        <> metavar "host" 
        <> help "katago host")
    <*> switch
      ( long "score-estimate"
          <> short 's'
          <> help "Whether to estimate the score of the game using KataGo")
      

