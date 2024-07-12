{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Kifu where
import Diagrams.TwoD.Grid ( gridWithHalves', placeDiagramOnGrid, GridOpts (..), annotate )
import           Diagrams.TwoD.Text
import           Diagrams.Prelude   (Any, Diagram, Path, QDiagram, Renderable,
                                     V2, circle, fc, lw, none, opacity, red, black, white, yellow,
                                     (#), (===), (|||), rect, centerXY, square, atop, Default (def), thin, r2, named, IsName, local, Colour, )
import           Diagrams.Backend.Rasterific (B, renderPdf)

import qualified Data.SGF  as SGF
import Data.List (sortBy, sortOn)
import Data.Ord (comparing, Down (Down))
import Goban (GoStone(Black, White))

cSize :: Double
cSize = 0.045

isBlack :: (GoStone, Integer,Integer, Integer) -> Bool
isBlack (color, _, _, _) = color == Black

isWhite :: (GoStone, Integer,Integer, Integer) -> Bool
isWhite (color, _, _, _) = color == White


mapColor :: (Ord a, Floating a) => GoStone -> Colour a
mapColor Black = black
mapColor White = white

tfm :: Integer -> Integer -> (Int,Int)
tfm x y = ((fromIntegral x*2)+1,(fromIntegral y*2)+1)

transformMovetoBoard :: (a, Integer, Integer, Integer) -> (Int,Int)
transformMovetoBoard (_, x',y', n) = tfm x' y'

woodenBoard ::  QDiagram B V2 Double Any
woodenBoard  = square 1.1  # lw none # fc yellow # opacity 0.5

-- myGridOpts :: GridOpts
myGridOpts = GridOpts
        { _gridLineWidth = thin
        , _gridXColour   = black
        , _gridYColour   = black
        , _gridLL        = r2 (1.0, 1.0)
        , _gridLR        = r2 (2.0, 1.0)
        , _gridUL        = r2 (1.0, 2.0)
        }

placeBlackStones n = let dgm = circle (cSize / 2) # fc black  # opacity 1.0 # lw 0.1
                      in placeDiagramOnGrid dgm


placeWhiteStones n = let dgm = circle (cSize / 2) # fc white  # opacity 1.0 # lw 0.2
                      in placeDiagramOnGrid dgm


txtPt :: String ->  QDiagram B V2 Double Any
txtPt t = text t # fontSize (local 0.023)

ann :: Int -> Int -> Colour Double -> String -> QDiagram B V2 Double Any -> QDiagram B V2 Double Any
ann x y color number = annotate number txtPt color x y

twoUp :: Diagram B -> Diagram B -> Diagram B
twoUp a b = a === b

fourUp :: Diagram B -> Diagram B -> Diagram B -> Diagram B -> Diagram B
fourUp a b c d = twoUp a b ||| twoUp c d

kifu :: [(GoStone, Integer, Integer, Integer)] -> Integer -> QDiagram B V2 Double Any
kifu moves size = centerXY boardDiagram <> centerXY woodenBoard
    where
        blackMoves = filter isWhite moves
        whiteMoves = filter isBlack moves
        blackPoints = map  transformMovetoBoard  blackMoves
        whitePoints = map  transformMovetoBoard  whiteMoves
        last5 = take 5 $ sortOn (Data.Ord.Down . (\(_,_, _, x) -> x)) moves
        last5Locations = map (\(stone,x,y,nbr) -> (stone, (x,y,nbr))) last5


        bd = gridWithHalves' myGridOpts  (fromIntegral size)  (fromIntegral size)
                            # placeBlackStones "123" blackPoints
                            # placeWhiteStones "321" whitePoints

        boardDiagram = foldl (\acc (color, (x,y,n)) -> let (x',y') = tfm x y
                                                        in acc # ann  x'  y' (mapColor color) (show n) ) bd last5Locations

