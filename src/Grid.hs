{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Grid where
-- import           Diagrams.Backend.Rasterific (B)
import           Diagrams.TwoD.Grid
import           Diagrams.TwoD.Text
-- import Diagrams.TwoD (circle)
import           Diagrams.Prelude   (Any, Diagram, Path, QDiagram, Renderable,
                                     V2, circle, fc, lw, none, opacity, red, black, white,
                                     (#))

import Data.SGF

cSize :: Double
cSize = 0.03

transformMovetoBoard :: (Integer, Integer) -> (Int,Int)
transformMovetoBoard (x',y') = let x = fromIntegral x'
                                   y = fromIntegral y' 
                                in ((x*2)+1,(y*2)+1)

exampleGrid :: Renderable (Path V2 Double) b => [(Data.SGF.Color, (Integer,Integer))]-> Int  -> QDiagram b V2 Double Any
exampleGrid moves size = gridWithHalves size size # bndPts tfmpoints
    where
        bndPts = placeDiagramOnGrid (circle (cSize / 2) # fc black  # opacity 1.0 # lw 0.1)
        placesPlayed = map snd  moves
        tfmpoints = map transformMovetoBoard placesPlayed
