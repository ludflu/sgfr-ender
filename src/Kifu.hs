{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ScopedTypeVariables              #-}

module Kifu where
import Diagrams.TwoD.Grid ( gridWithHalves, gridWithHalves', placeDiagramOnGrid, GridOpts (..), annotate )
import           Diagrams.TwoD.Text
import           Diagrams.Prelude   (Any, Diagram, Path, QDiagram, Renderable,
                                     V2, circle, fc, lw, none, opacity, red, black, white, yellow,
                                     (#), (===), (|||), rect, centerXY, square, atop, Default (def), thin, r2, named, IsName, local, Colour, hcat', (.~), (&), padX, alignB, )
import           Diagrams.Backend.Rasterific (B, renderPdf)

import qualified Data.SGF  as SGF
import Data.List (sortBy, sortOn)
import Data.Ord (comparing, Down (Down))
import Goban (GoStone(Black, White))
import Diagrams (with, sep, vcat', alignL, pad)
import qualified Data.Map as M

cSize :: Double
cSize = 0.045

isBlack :: (GoStone, Integer,Integer, Integer) -> Bool
isBlack (color, _, _, _) = color == Black

isWhite :: (GoStone, Integer,Integer, Integer) -> Bool
isWhite (color, _, _, _) = color == White

flipColor :: (Ord a, Floating a) => GoStone -> Colour a
flipColor Black = white
flipColor White = black

tfm :: Integer -> Integer -> (Int,Int)
tfm x y = ((fromIntegral x*2)+1,(fromIntegral y*2)+1)

transformMovetoBoard :: (a, Integer, Integer, Integer) -> (Int,Int)
transformMovetoBoard (_, x',y', n) = tfm x' y'

woodenBoard ::  QDiagram B V2 Double Any
woodenBoard  = square 1.15  # lw none # fc yellow # opacity 0.5

myGridOpts :: (Floating n, Ord n) => GridOpts n
myGridOpts = GridOpts
        { _gridLineWidth = thin
        , _gridXColour   = black
        , _gridYColour   = black
        , _gridLL        = r2 (1.0, 1.0)
        , _gridLR        = r2 (2.0, 1.0)
        , _gridUL        = r2 (1.0, 2.0)
        }

placeBlackStones :: (IsName nm, Renderable (Path V2 Double) b) => [nm] -> QDiagram b V2 Double Any -> QDiagram b V2 Double Any
placeBlackStones = let dgm = circle (cSize / 2) # fc black  # opacity 1.0 # lw 0.1
                      in placeDiagramOnGrid dgm


placeWhiteStones :: (IsName nm, Renderable (Path V2 Double) b) => [nm] -> QDiagram b V2 Double Any -> QDiagram b V2 Double Any
placeWhiteStones = let dgm = circle (cSize / 2) # fc white # opacity 1.0 # lw 0.2
                      in placeDiagramOnGrid dgm

starPoints :: (IsName nm, Renderable (Path V2 Double) b) => [nm] -> QDiagram b V2 Double Any -> QDiagram b V2 Double Any
starPoints = let dgm = circle (cSize / 5) # fc black # opacity 1.0 # lw 0.2
                      in placeDiagramOnGrid dgm


moveNumberLabel :: String ->  QDiagram B V2 Double Any
moveNumberLabel t   -- as the number of the move increases, the label should get smaller
        | length t == 1 = text t # fontSize (local 0.026)
        | length t == 2 = text t # fontSize (local 0.020)
        | length t > 2  = text t # fontSize (local 0.016)


vertLabelLocations :: Integer -> [(Integer, Integer, String)]
vertLabelLocations boardSize = let vert = reverse [0..boardSize-1]
                      in map (\t -> (0, t, show (boardSize-t))) vert

charToString :: Char -> String
charToString c = [c]

boardLetters = map charToString $ filter (/= 'I') ['A'..'T']

makeBoardPoint :: Integer -> Integer -> Integer -> String
makeBoardPoint boardSize x y = (boardLetters !! fromInteger x) ++ show (fromInteger boardSize - y)

moveNameLookup :: Integer -> M.Map (Integer, Integer) String
moveNameLookup boardSize = let pointTuples = [ ((x,y), makeBoardPoint boardSize x y ) | x <- [0..boardSize], y <- [0..boardSize]]
                            in M.fromList pointTuples

horzLabelLocations :: Integer -> [(Integer, Integer, String)]
horzLabelLocations boardSize = let horz = [0..boardSize-1]                                   
                                --    noI = take (fromInteger boardSize)  boardLetters
                                in map (\t -> (t, boardSize-1, boardLetters !! fromInteger t)) horz


ann :: Int -> Int -> Colour Double -> String -> Diagram B -> Diagram B
ann x y color number = annotate number moveNumberLabel color x y

annSide :: Int -> Int -> Colour Double -> String -> Diagram B -> Diagram B
annSide x y color number = let lbn n = text (n++ "            ") # fontSize (local 0.020) # alignL
                            in annotate number lbn color x y

annBottom :: Int -> Int -> Colour Double -> String -> Diagram B -> Diagram B
annBottom x y color number = let lbn n = vcat' (with & sep .~ 0.04) [ text " " # fontSize (local 0.020) , text n # fontSize (local 0.020) ]
                            in annotate number lbn color x y

twoUp :: Diagram B -> Diagram B -> Diagram B
twoUp a b = vcat' (with & sep .~ 0.15) [a,b]

fourUp :: Diagram B -> Diagram B -> Diagram B -> Diagram B -> Diagram B
fourUp a b c d = let top = hcat' (with & sep .~ 0.15) [a,b]
                     bottom = hcat' (with & sep .~ 0.15) [c,d]
                  in twoUp top bottom

starPointLocations :: [(Int, Int)]
starPointLocations = map (uncurry tfm) ([(3,3),(15,15),(15,3),(3,15)] ++ [ (3,9),(9,3),(9,9),(3,3)]  ++ [(15,9), (9,15),(9,9),(15,15)])

kifu :: [(GoStone, Integer, Integer, Integer)] -> Integer -> QDiagram B V2 Double Any
kifu moves boardSize = centerXY labeledXBoard <> centerXY woodenBoard
    where
        blackMoves = filter isBlack moves
        whiteMoves = filter isWhite moves
        blackPoints = map transformMovetoBoard blackMoves
        whitePoints = map transformMovetoBoard whiteMoves
        last5 = take 5 $ sortOn (Data.Ord.Down . (\(_,_,_,x) -> x)) moves
        last5Locations = map (\(stone,x,y,nbr) -> (stone, (x,y,nbr))) last5
        bd = gridWithHalves' myGridOpts  (fromIntegral boardSize-1) (fromIntegral boardSize-1)
                            # starPoints starPointLocations
                            # placeWhiteStones whitePoints
                            # placeBlackStones blackPoints

        boardDiagram = foldl (\acc (color, (x,y,n)) -> let (x',y') = tfm x y
                                                        in acc # ann x' y' (flipColor color) (show n) ) bd last5Locations


        labeledYBoard  = foldl (\acc (x,y,n) -> let (x',y') = tfm x y
                                              in acc # annSide x' y' black n)  boardDiagram $ vertLabelLocations boardSize

        labeledXBoard  = foldl (\acc (x,y,n) -> let (x',y') = tfm x y
                                              in acc # annBottom x' y' black n)  labeledYBoard $ horzLabelLocations boardSize
