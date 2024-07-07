{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Data.ByteString (ByteString, getContents, unpack)
import Data.List hiding ((!!))
import Data.SGF
import Data.Tree
import Diagrams.Backend.Rasterific
import Diagrams.Backend.SVG hiding (B)
import Diagrams.Prelude
import Grid (exampleGrid)
import Prelude hiding (getContents, (!!))

(!!) = genericIndex

kifu :: Diagram B
kifu = exampleGrid 18 18

main :: IO ()
main = renderPdf 200 200 "output.pdf" (dims2D 200 200) kifu

-- main = renderSVG "output.svg" (dims2D 200 200) kifu