{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import           Grid                        (kifu)
import           Diagrams.Backend.Rasterific (B, renderPdf)
import Diagrams.TwoD (dims2D)
import SgfReader (readSgf, showMoves, renderReady)
import Goban ( GoStone(White, Black), emptyBoard, playMoves, getAllStones, GoPoint (x, y) )
import qualified Data.SGF as SGF
import Control.Monad.State ( execState )
import Diagrams (Renderable)
import Diagrams.Prelude

convertToMoves ::  [(SGF.Color, (Integer, Integer))] -> [(GoStone, Int,Int)]
convertToMoves = map (\(color, (x,y)) -> (if color == SGF.Black then Black else White, fromIntegral x, fromIntegral y))

mygoban :: Renderable (Path V2 Double) B => [(SGF.Color, (Integer,Integer))] -> Int  -> QDiagram B V2 Double Any
mygoban = kifu

stonePlacement:: [(GoPoint,GoStone)] -> [ (SGF.Color, (Integer, Integer))]
stonePlacement = map (\(point, stone) -> (if stone == Black then SGF.Black else SGF.White, ( toInteger $ x point, toInteger $  y point)))

main :: IO ()
main = do
  sgf <- readSgf "65761210-307-mannesmann-ludflu215.sgf"
  let boardSize = 18
      moves = renderReady sgf
      gobanMoves = convertToMoves moves
      initialGoban = emptyBoard boardSize
      finalBoard = execState (playMoves gobanMoves) initialGoban
      onlyStones = getAllStones finalBoard
      gomoves = stonePlacement onlyStones 
      kifuDiagram = mygoban gomoves boardSize
  print $ show onlyStones
  renderPdf 200 200 "output.pdf" (dims2D 200 200) kifuDiagram
