{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module GoBan where

import           Data.ByteString             (ByteString, getContents, pack,
                                              unpack)
import qualified Data.ByteString.UTF8        as BSU
import           Data.List                   hiding ((!!))
import           Data.SGF
import           Data.Tree
import           Grid                        (exampleGrid)
import           Prelude                     hiding (getContents, (!!))
import Data.Maybe
import qualified Data.Map as M

--this module is used to calculate what stones to
-- take off the board when a new stone is placed

data GoStone = Black | White deriving (Eq, Show)
data GoPoint =  GoPoint { x::Int, y::Int }deriving (Eq, Show)
data BoardState = BoardState { 
  board :: M.Map (Int,Int) GoStone,
  boardSize :: Int
} deriving (Show)

isOnBoard :: BoardState -> GoPoint ->  Bool
isOnBoard boardState (GoPoint x y)  = x >= 0 && x < boardSize boardState && y >= 0 && y < boardSize boardState 

neighbors :: BoardState -> GoPoint -> [GoPoint]
neighbors boardState (GoPoint x y) = 
  let ns = [GoPoint (x+1) y, GoPoint (x-1) y, GoPoint x (y+1), GoPoint x (y-1)]
   in filter (isOnBoard boardState) ns

-- given boardState and a point, 
-- return the list of points 
-- connected by same-colored stones
findDragon :: BoardState -> GoPoint -> [GoPoint]
findDragon boardState point = []

-- given boardState and a point,
-- find all adjacent enemy dragons
findAdjacentEnemyDragons :: BoardState -> GoPoint -> [[GoPoint]]
findAdjacentEnemyDragons boardState point = []

calcLiberties :: BoardState -> GoPoint -> Int
calcLiberties boardState point = 0


