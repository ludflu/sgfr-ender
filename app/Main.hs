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


convertToMoves ::  [(SGF.Color, (Integer, Integer))] -> [(GoStone, Int,Int)]
convertToMoves = map (\(color, (x,y)) -> (if color == SGF.Black then Black else White, fromIntegral x, fromIntegral y))

mygoban ::  [(SGF.Color, (Integer,Integer))] -> Int  -> Diagram B
mygoban = kifu

stonePlacement:: [(GoPoint,GoStone)] -> [ (SGF.Color, (Integer, Integer))]
stonePlacement = map (\(point, stone) -> (if stone == Black then SGF.Black else SGF.White, ( toInteger $ x point, toInteger $  y point)))

graduatedMoveList :: Int -> [a] -> [[a]]
graduatedMoveList step items = let moveCount = length items
                                   moveExtents = [i * step | i <- [0..moveCount]]
                                   in map (\extent -> take extent items) moveExtents



run :: RenderOpts -> IO ()
run renderOpts = do
  sgf <- readSgf (input renderOpts) --"65761210-307-mannesmann-ludflu215.sgf"
  let boardSize = 18
      moves =  renderReady sgf
      gobanMoves = convertToMoves moves
      initialGoban = emptyBoard boardSize
      finalBoard = execState (playMoves gobanMoves) initialGoban
      gostones = stonePlacement $ getAllStones finalBoard
      kifuDiagram = mygoban gostones boardSize
      outpath = fromMaybe "goban.pdf" (output renderOpts)
  renderPdf 200 200 outpath (dims2D 200 200) kifuDiagram


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