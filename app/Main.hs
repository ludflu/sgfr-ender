{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Data.ByteString (ByteString, getContents, pack, unpack)
-- import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.UTF8 as BSU
import Data.List hiding ((!!))
import Data.SGF
import Data.Tree
import Diagrams.Backend.Rasterific
import Diagrams.Backend.SVG hiding (B)
import Diagrams.Prelude hiding (Point, pad)
import Grid (exampleGrid)
import Prelude hiding (getContents, (!!))

(!!) = genericIndex

kifu :: Diagram B
kifu = exampleGrid 18 18

grabTree :: [Word8] -> TreeGo
grabTree s = case runParser collection () "stdin" s of
  Right ([Game {tree = TreeGo t}], _) -> t

grabMoves :: TreeGo -> [MoveGo]
grabMoves
  n = [move | Right Move {move = Just (color, move)} <- mainLine]
    where
      mainLine = map action . head . transpose . levels $ n

parse :: ByteString -> [MoveGo]
parse = grabMoves . grabTree . unpack

coordinates :: [Char]
coordinates = delete 'I' ['A' .. 'Z']

showPoint :: Point -> String
showPoint (x, y) = coordinates !! x : show (19 - y)

pad :: String -> String
pad s = s ++ replicate (4 - length s) ' '

showMove :: MoveGo -> String
showMove Pass = "pass"
showMove (Play p) = pad (showPoint p)

showMoves :: [MoveGo] -> String
showMoves = unlines . showMoves' 1
  where
    showMoves' n [] = []
    showMoves' n [m] = unwords [show n ++ ".", showMove m] : []
    showMoves' n (m : m' : ms) = unwords [show n ++ ".", showMove m, showMove m'] : showMoves' (n + 2) ms

-- main :: IO ()
-- main = renderPdf 200 200 "output.pdf" (dims2D 200 200) kifu

main :: IO ()
main = do
  f <- readFile "65761210-307-mannesmann-ludflu215.sgf"
  let fs = BSU.fromString f

  putStrLn $ showMoves (parse fs)
