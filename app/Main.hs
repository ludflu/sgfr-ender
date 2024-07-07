{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import           Grid                        (exampleGrid)
import           Diagrams.Backend.Rasterific (B, renderPdf)
import Diagrams.TwoD (dims2D)
import SgfReader (readSgf)


-- kifu = exampleGrid 18 18

-- main :: IO ()
-- main = renderPdf 200 200 "output.pdf" (dims2D 200 200) kifu



main :: IO ()
main = do
  sgf <- readSgf "65761210-307-mannesmann-ludflu215.sgf"
  print $ show sgf
--   f <- readFile "65761210-307-mannesmann-ludflu215.sgf"
--   let fs = BSU.fromString f
--   putStrLn $ showMoves (parse fs)
