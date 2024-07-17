{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ScopedTypeVariables       #-}


module Main where

import Kifu                        (kifu, twoUp, fourUp)
import RenderOpts
import Diagrams.Backend.Rasterific (B, renderPdf, renderPdfBSWithDPI)
import Diagrams.TwoD (dims2D)
import SgfReader (readSgf, showMoves)
import Goban ( GoStone(White, Black), emptyBoard, playMoves, getAllStones, GoPoint (x, y), BoardState (moveNumberMap) )
import qualified Data.Map as M
import qualified Data.SGF as SGF
import Control.Monad.State ( execState )
import Diagrams (Renderable)
import Diagrams.Prelude hiding (output)
import Diagrams.Backend.Rasterific.CmdLine
import KataGoApi (scoreAllMoves)
import Data.Maybe (catMaybes, fromMaybe)
import Options.Applicative
  ( Alternative (empty),
    execParser,
    fullDesc,
    header,
    helper,
    info,
    progDesc,
    (<**>),
  )
import Data.List.Split (chunksOf, divvy)
import Data.List ( tails )
import Control.Arrow hiding ((|||))
import Text.Printf (printf)
mapColor :: SGF.Color -> GoStone
mapColor SGF.Black = Black
mapColor SGF.White = White

convertColor ::  GoStone -> SGF.Color
convertColor Black=SGF.Black
convertColor White=SGF.White


mg :: SGF.Color -> SGF.MoveGo  -> Maybe (GoStone, Integer,Integer)
mg c (SGF.Play (x,y)) = Just (mapColor c,x,y)
mg c SGF.Pass = Nothing

convertToMoves ::  [(SGF.Color, SGF.MoveGo)] -> [(GoStone, Integer, Integer, Integer)]
convertToMoves mvs =  let onlyMoves = catMaybes $ map (\(c, m) -> mg c m) mvs
                          numberedMoves = zip onlyMoves [1..]
                       in map (\((s,x,y),n) -> (s,x,y,n)) numberedMoves

stonePlacement:: [(GoPoint,GoStone)] ->  M.Map GoPoint Integer -> [ (GoStone, Integer, Integer, Integer)]
stonePlacement stones moveMap = map (\(point, stone) -> ( stone,   toInteger $ x point, toInteger $  y point, toInteger $ moveMap M.! point  ) ) stones

graduatedMoveList :: Int -> [a] -> [[a]]
graduatedMoveList step items = let moveCount = length items
                                   stepCount = moveCount `div` step
                                   moveExtents = [i * step | i <- [0..stepCount]]
                                   in tail $ map (`take` items) moveExtents

dpi = 96
widthInInches = 8.5
heightInInches = 11.0
widthInPixels :: Int = round $ widthInInches * dpi
heightInPixels ::Int = round $ heightInInches * dpi

diagramSize :: Double = fromIntegral widthInPixels

renderDiagram :: FilePath -> Diagram B -> IO ()
renderDiagram outfile d =  let centeredD = d # centerXY # pad 1.1
                            in renderPdf widthInPixels heightInPixels outfile (dims2D diagramSize diagramSize) centeredD

renderDiagrams :: FilePath -> [Diagram B] -> IO ()
renderDiagrams outfile [kifu] = renderDiagram outfile kifu
renderDiagrams outfile [a,b] = renderDiagram outfile $ twoUp a b
renderDiagrams outfile [a,b,c] = renderDiagram outfile $ fourUp a b c mempty
renderDiagrams outfile [a,b,c,d] = renderDiagram outfile $ fourUp a b c d

buildDiagram :: Integer  ->  [(GoStone, Integer, Integer, Integer)] -> [Double] ->Diagram B
buildDiagram boardSize moves scores = let
      initialGoban = emptyBoard boardSize
      finalBoard = execState (playMoves moves) initialGoban
      gostones = stonePlacement (getAllStones finalBoard) (moveNumberMap finalBoard)
   in kifu gostones boardSize scores


makeFileName :: String -> Integer -> String
makeFileName prefix pageNumber = let pnum = printf "%05d" pageNumber
                                  in prefix ++ "-" ++ pnum ++ ".pdf"

getScore :: Bool -> Integer -> [(GoStone, Integer, Integer, Integer)] -> IO [Double]
getScore scoringRequested boardSize moves = if scoringRequested then 
                                                scoreAllMoves "localhost" 2178 boardSize moves
                                            else return []


calculateMoveStrength :: [Double] -> [Double]
calculateMoveStrength  pointEstimates = let first = head pointEstimates
                                            moveWindow = divvy 2 1 pointEstimates
                                            moveStrength = map (\(x:y:_) -> y-x ) moveWindow
                                         in first : moveStrength

run :: RenderOpts  -> IO ()
run renderOpts  = do
  (boardSize,sgf) <- readSgf (input renderOpts)
  let scoringRequested = scoreEstimate renderOpts
  let process = convertToMoves >>> graduatedMoveList (movesPerDiagram renderOpts)
      movestack = process sgf
  scores <-  getScore scoringRequested  boardSize $ last movestack  

  let 
      numberedMoveList = zip [1..] movestack
      allKifus = map (\(i, moves) -> buildDiagram boardSize moves scores) numberedMoveList
      chunkedKifus = zip [1..] $ chunksOf (diagramsPerPage renderOpts) allKifus
 
   in do 
         mapM_ (\(i, kifu) -> renderDiagrams (makeFileName (output renderOpts) i) kifu) chunkedKifus

main :: IO ()
main = run =<< execParser opts
  where
    opts =
      info
        (parseOpts <**> helper)
        ( fullDesc
            <> progDesc "render the sgf file to pdf"
            <> header "sgf-render - render sgf files to pdf"
        )