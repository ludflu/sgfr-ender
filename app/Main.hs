{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Kifu                        (kifu, twoUp, fourUp)
import RenderOpts
import Diagrams.Backend.Rasterific (B, renderPdf)
import Diagrams.TwoD (dims2D)
import SgfReader (readSgf, showMoves, renderReady)
import Goban ( GoStone(White, Black), emptyBoard, playMoves, getAllStones, GoPoint (x, y), BoardState (moveNumberMap) )
import qualified Data.Map as M
import qualified Data.SGF as SGF
import Control.Monad.State ( execState )
import Diagrams (Renderable)
import Diagrams.Prelude hiding (output)
import Diagrams.Backend.Rasterific.CmdLine
import Data.Maybe (mapMaybe, fromMaybe)
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
import Data.List.Split (chunksOf)


mapColor :: SGF.Color -> GoStone
mapColor SGF.Black = Black
mapColor SGF.White = White

convertColor ::  GoStone -> SGF.Color
convertColor Black=SGF.Black 
convertColor White=SGF.White


convertToMoves ::  [(SGF.Color, (Integer, Integer))] -> [(GoStone, Int, Int, Int)]
convertToMoves mvs = let numberedMoves = zip [1..] mvs
                      in
                         map (\(n, (color, (x,y))) -> (mapColor color, fromIntegral x, fromIntegral y, n)) numberedMoves

mygoban ::  [(SGF.Color, (Integer, Integer, Integer))] -> Int  -> Diagram B
mygoban = kifu

stonePlacement:: [(GoPoint,GoStone)] ->  M.Map GoPoint Int -> [ (SGF.Color, (Integer, Integer, Integer))]
stonePlacement stones moveMap = map (\(point, stone) -> (convertColor stone,  ( toInteger $ x point, toInteger $  y point, toInteger $ moveMap M.!  point ) ) ) stones

graduatedMoveList :: Int -> [a] -> [[a]]
graduatedMoveList step items = let moveCount = length items
                                   stepCount = moveCount `div` step
                                   moveExtents = [i * step | i <- [0..stepCount]]
                                   in tail $ map (`take` items) moveExtents

renderDiagram :: FilePath -> Diagram B -> IO ()
renderDiagram outfile kifuDiagram = renderPdf 200 200 outfile (dims2D 200 200) kifuDiagram

renderDiagrams :: FilePath -> [Diagram B] -> IO ()
renderDiagrams outfile [kifu] = renderDiagram outfile kifu
renderDiagrams outfile [a,b] = renderDiagram outfile $ twoUp a b
renderDiagrams outfile [a,b,c] = renderDiagram outfile $ a === b ||| c
renderDiagrams outfile [a,b,c,d] = renderDiagram outfile $ fourUp a b c d

buildDiagram :: Int  ->  [(GoStone, Int, Int, Int)] -> Diagram B
buildDiagram boardSize moves = let 
      initialGoban = emptyBoard boardSize
      finalBoard = execState (playMoves moves) initialGoban
      gostones = stonePlacement (getAllStones finalBoard) (moveNumberMap finalBoard)
   in mygoban gostones boardSize

buildAndRenderDiagram :: Int -> FilePath ->  [(GoStone, Int, Int, Int)] -> IO ()
buildAndRenderDiagram boardSize outfile moves = let kifu = buildDiagram boardSize moves
                                                 in renderDiagram outfile kifu

run :: RenderOpts  -> IO ()
run renderOpts  = do
  sgf <- readSgf (input renderOpts)
  let boardSize = 18
      pdfDims = 200
      moves =  renderReady sgf
      gobanMoves = convertToMoves moves
      outfile = output renderOpts
      movestack = graduatedMoveList (movesPerDiagram renderOpts) gobanMoves
      numberedMoveList = zip [1..] movestack
      allKifus = map (\(i, moves) -> buildDiagram boardSize moves) numberedMoveList
      chunkedKifus = zip [1..] $ chunksOf (diagramsPerPage renderOpts) allKifus
   in mapM_ (\(i, kifu) -> renderDiagrams (outfile ++ "-" ++ show i ++ ".pdf") kifu) chunkedKifus

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