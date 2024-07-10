{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Grid                        (kifu)
import RenderOpts
import Diagrams.Backend.Rasterific (B, renderPdf)
import Diagrams.TwoD (dims2D)
import SgfReader (readSgf, showMoves, renderReady)
import Goban ( GoStone(White, Black), emptyBoard, playMoves, getAllStones, GoPoint (x, y) )
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

flipColor :: SGF.Color -> GoStone
flipColor SGF.Black = White
flipColor SGF.White = Black

convertColor ::  GoStone -> SGF.Color
convertColor  Black=SGF.Black 
convertColor White= SGF.White 

convertToMoves ::  [(SGF.Color, (Integer, Integer))] -> [(GoStone, Int, Int, Int)]
convertToMoves mvs = let numberedMoves = zip [1..] mvs
                      in
                         map (\(n, (color, (x,y))) -> (flipColor color, fromIntegral x, fromIntegral y, n)) numberedMoves

mygoban ::  [(SGF.Color, (Integer, Integer, Integer))] -> Int  -> Diagram B
mygoban = kifu

stonePlacement:: [(GoPoint,GoStone)] -> [ (SGF.Color, (Integer, Integer, Integer))]
stonePlacement = map (\(point, stone) -> (convertColor stone, ( toInteger $ x point, toInteger $  y point, 123)))

graduatedMoveList :: Int -> [a] -> [[a]]
graduatedMoveList step items = let moveCount = length items
                                   stepCount = moveCount `div` step
                                   moveExtents = [i * step | i <- [0..stepCount]]
                                   in map (`take` items) moveExtents


makeDiagram :: Int -> FilePath ->  [(GoStone, Int, Int, Int)] -> IO ()
makeDiagram boardSize outfile moves = let 
   initialGoban = emptyBoard boardSize
   finalBoard = execState (playMoves moves) initialGoban
   gostones = stonePlacement $ getAllStones finalBoard
   kifuDiagram = mygoban gostones boardSize
   in renderPdf 200 200 outfile (dims2D 200 200) kifuDiagram


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
   in mapM_ (\(i, moves) -> makeDiagram boardSize (outfile ++ "-" ++ show i ++ ".pdf") moves) numberedMoveList

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