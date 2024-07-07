{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Grid where
import           Diagrams.TwoD.Grid
import           Diagrams.TwoD.Text
import           Diagrams.Prelude   (Any, Diagram, Path, QDiagram, Renderable,
                                     V2, circle, fc, lw, none, opacity, red, black, white,
                                     (#))

import Data.SGF ( Color, Color(White), Color(Black) ) 

cSize :: Double
cSize = 0.03

isBlack :: (Color, (Integer,Integer)) -> Bool
isBlack (color, _) = color == Black

isWhite :: (Color, (Integer,Integer)) -> Bool
isWhite (color, _) = color == White

transformMovetoBoard :: (Integer, Integer) -> (Int,Int)
transformMovetoBoard (x',y') = let x = fromIntegral x'
                                   y = fromIntegral y' 
                                in ((x*2)+1,(y*2)+1)

exampleGrid :: Renderable (Path V2 Double) b => [(Data.SGF.Color, (Integer,Integer))]-> Int  -> QDiagram b V2 Double Any
exampleGrid moves size = gridWithHalves size size 
        # placeBlackStones blackPoints
        # placeWhiteStones whitePoints
    where
        placeBlackStones = placeDiagramOnGrid (circle (cSize / 2) # fc black  # opacity 1.0 # lw 0.1)
        placeWhiteStones = placeDiagramOnGrid (circle (cSize / 2) # fc white  # opacity 1.0 # lw 0.1)
        blackMoves = filter isBlack moves
        whiteMoves = filter isWhite moves
        blackPoints = map  (transformMovetoBoard . snd)  blackMoves
        whitePoints = map  (transformMovetoBoard . snd)  whiteMoves

        
