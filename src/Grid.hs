{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Grid where

import Diagrams.TwoD.Grid
import Diagrams.TwoD.Text
-- import Diagrams.TwoD (circle)
import Diagrams.Prelude ( red, lw, none, opacity, fc, circle, (#), Diagram, V2, Renderable, Path, QDiagram, Any )

cSize :: Double
cSize = 0.03

-- bndPts :: [a] -> Diagram B -> Diagram B

-- example :: (Renderable (Text Double) b, Renderable (Path V2 Double) b) =>
--           Int -> Int -> QDiagram b V2 Double Any

-- exampleGrid :: (Renderable (Text Double) b, Renderable (Path V2 Double) b) =>
        --    Int -> Int -> QDiagram b V2 Double Any
exampleGrid n m = gridWithHalves n m # bndPts [ (1::Int,1::Int)]
    where 
        bndPts = placeDiagramOnGrid (circle (cSize / 2) # fc red  # opacity 0.5 # lw none)
