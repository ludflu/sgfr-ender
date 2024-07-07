{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Grid where
-- import           Diagrams.Backend.Rasterific (B)
import           Diagrams.TwoD.Grid
import           Diagrams.TwoD.Text
-- import Diagrams.TwoD (circle)
import           Diagrams.Prelude   (Any, Diagram, Path, QDiagram, Renderable,
                                     V2, circle, fc, lw, none, opacity, red,
                                     (#))

cSize :: Double
cSize = 0.03

-- bndPts :: [a] -> Diagram B -> Diagram B

-- example :: (Renderable (Text Double) b, Renderable (Path V2 Double) b) =>
--           Int -> Int -> QDiagram b V2 Double Any

-- exampleGrid :: (Renderable (Text Double) b, Renderable (Path V2 Double) b) =>
        --    Int -> Int -> QDiagram b V2 Double Any

-- kifu :: Diagram B
-- kifu = exampleGrid 18 18


pnts = let ps = [(x,y) | x <- [1::Int ..19], y <- [1::Int ..19]]
           doubleTuple (x,y) = ((x*2)-1, (y*2)-1)
        in map doubleTuple ps
        
exampleGrid :: Renderable (Path V2 Double) b => Int -> Int -> QDiagram b V2 Double Any
exampleGrid n m = gridWithHalves n m # bndPts pnts
    where
        bndPts = placeDiagramOnGrid (circle (cSize / 2) # fc red  # opacity 0.5 # lw none)
