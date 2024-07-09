module RenderOpts where

import Data.Maybe 
import Options.Applicative

data RenderOpts = RenderOpts {
    movesPerDiagram :: Int,
    diagramsPerPage :: Int,
    input :: Maybe FilePath,
    output :: Maybe FilePath
    } deriving (Show)


parseOpts :: Parser RenderOpts
parseOpts = RenderOpts <$>
  option auto
      ( long "movesPerDiagram"
          <> metavar "movesPerDiagram"
          <> showDefault
          <> value 5
          <> help "how many moves to show in each diagram")
    <*> option auto
       (long "diagramsPerPage"
          <> metavar "diagramsPerPage"
          <> showDefault
          <> value 4
          <> help "how many diagrams to show on each page")
    <*> optional (strOption $ long "input" <> metavar "input" <> help "input file")
    <*> optional (strOption $ long "output" <> metavar "output" <> help "output file")

