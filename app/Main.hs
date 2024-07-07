{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import           Grid                        (exampleGrid)
import           Diagrams.Backend.Rasterific (B, renderPdf)
import Diagrams.TwoD (dims2D)
import SgfReader (readSgf, showMoves, renderReady)


main :: IO ()
main = do
  sgf <- readSgf "65761210-307-mannesmann-ludflu215.sgf"
  let moves = renderReady sgf
      kifu = exampleGrid moves 18
  print $ show moves
  renderPdf 200 200 "output.pdf" (dims2D 200 200) kifu
