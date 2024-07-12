module RenderOpts where

import Data.Maybe 
import Options.Applicative

data RenderOpts = RenderOpts {
    movesPerDiagram :: Int,
    diagramsPerPage :: Int,
    input ::  FilePath,
    output :: FilePath
    } deriving (Show)


parseOpts :: Parser RenderOpts
parseOpts = RenderOpts <$>
  option auto (
       long "movesPerDiagram"
          <> metavar "movesPerDiagram"
          <> showDefault
          <> value 5
          <> help "how many moves to show in each diagram")
    <*> option auto 
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

