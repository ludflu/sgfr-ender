{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Grid where

import Diagrams.TwoD.Grid
import Diagrams.TwoD.Text

-- example :: (Renderable (Text Double) b, Renderable (Path V2 Double) b) =>
--           Int -> Int -> QDiagram b V2 Double Any
exampleGrid n m = (gridWithHalves n m)
