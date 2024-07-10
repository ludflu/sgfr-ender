-- {-# LANGUAGE FlexibleContexts      #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}


{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Grid where
import Diagrams.TwoD.Grid ( gridWithHalves', placeDiagramOnGrid, GridOpts (..) )
import           Diagrams.TwoD.Text
import           Diagrams.Prelude   (Any, Diagram, Path, QDiagram, Renderable,
                                     V2, circle, fc, lw, none, opacity, red, black, white, yellow,
                                     (#), rect, centerXY, square, atop, Default (def), thin, r2, named, )
import           Diagrams.Backend.Rasterific (B, renderPdf)

import Data.SGF ( Color, Color(White), Color(Black) ) 

cSize :: Double
cSize = 0.045

isBlack :: (Color, (Integer,Integer, Integer)) -> Bool
isBlack (color, _) = color == Black

isWhite :: (Color, (Integer,Integer, Integer)) -> Bool
isWhite (color, _) = color == White

transformMovetoBoard :: (Integer, Integer, Integer) -> (Int,Int)
transformMovetoBoard (x',y', n) = let x = fromIntegral x'
                                      y = fromIntegral y' 
                                in ((x*2)+1,(y*2)+1)

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

blackStone n = placeDiagramOnGrid (text (show n) # fontSizeL 0.015 # fc white <> circle (cSize / 2) # fc black  # opacity 1.0 # lw 0.1) 

whiteStone n = placeDiagramOnGrid (text (show n) # fontSizeL 0.015 # fc black <> circle (cSize / 2) # fc white  # opacity 1.0 # lw 0.2)

kifu :: [(Color, (Integer, Integer, Integer))] -> Int -> QDiagram B V2 Double Any
kifu moves size = centerXY boardDiagram <> centerXY woodenBoard
    where
        placeBlackStones = blackStone 123 
        placeWhiteStones = whiteStone 321
        blackMoves = filter isWhite moves
        whiteMoves = filter isBlack moves
        blackPoints = map  (transformMovetoBoard . snd)  blackMoves
        whitePoints = map  (transformMovetoBoard . snd)  whiteMoves
        boardDiagram = gridWithHalves' myGridOpts size size 
                            # placeBlackStones blackPoints
                            # placeWhiteStones whitePoints

        
