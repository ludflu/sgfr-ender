{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Data.ByteString (ByteString, getContents, unpack)
import Data.List hiding ((!!))
import Data.SGF
import Data.Tree
-- import Diagrams.Backend.PDF
import Diagrams.Backend.SVG
import Diagrams.Prelude
import Grid (exampleGrid)
import Prelude hiding (getContents, (!!))

(!!) = genericIndex

kifu :: Diagram B
kifu = exampleGrid 18 18

main :: IO ()
-- main = renderPDF "output.pdf" (dims2D 200 200) myDiagram
main = renderSVG "output.svg" (dims2D 200 200) kifu