{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import           Grid                        (exampleGrid)
import           Diagrams.Backend.Rasterific (B, renderPdf)
import Diagrams.TwoD (dims2D)
import SgfReader (readSgf, showMoves, renderReady)
import Goban ( GoStone(White, Black), emptyBoard, playMoves, getAllStones )
import qualified Data.SGF as SGF
import Control.Monad.State ( execState )

convertToMoves ::  [(SGF.Color, (Integer, Integer))] -> [(GoStone, Int,Int)]
convertToMoves = map (\(color, (x,y)) -> (if color == SGF.Black then Black else White, fromIntegral x, fromIntegral y))

main :: IO ()
main = do
  sgf <- readSgf "65761210-307-mannesmann-ludflu215.sgf"
  let boardSize = 18
      moves = renderReady sgf
      gobanMoves = convertToMoves moves
      initialGoban = emptyBoard boardSize
      finalBoard = execState (playMoves gobanMoves) initialGoban
      onlyStones = getAllStones finalBoard
      kifuDiagram = kifu moves boardSize
  print $ show onlyStones
  -- renderPdf 200 200 "output.pdf" (dims2D 200 200) kifu
