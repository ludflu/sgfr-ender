{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module Goban where

import           Data.ByteString             (ByteString, getContents, pack,
                                              unpack)
import qualified Data.ByteString.UTF8        as BSU
import           Data.List                   hiding ((!!))
--import           Data.SGF
import           Data.Tree
import           Grid                        (exampleGrid)
import           Prelude                     hiding (getContents, (!!))
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Diagrams (place)
-- import qualified Diagrams.BoundingBox as S

--this module is used to calculate what stones to
-- take off the board when a new stone is placed

data GoStone = Black | White | Empty deriving (Eq, Show)
data GoPoint =  GoPoint { x::Int, y::Int }deriving (Eq, Show, Ord)
data BoardState = BoardState {
  board :: M.Map GoPoint GoStone,
  boardSize :: Int
} deriving (Show)

opposite :: GoStone -> GoStone
opposite Black = White
opposite White = Black
opposite Empty = Empty

isOnBoard :: Int -> GoPoint ->  Bool
isOnBoard bSize (GoPoint x y)  = x >= 0 && x < bSize && y >= 0 && y < bSize

neighbors :: GoPoint -> State BoardState [GoPoint]
neighbors  (GoPoint x y) = do boardState <- get
                              let ns = [GoPoint (x+1) y, GoPoint (x-1) y, GoPoint x (y+1), GoPoint x (y-1)]
                                  onlyOnboard = filter (isOnBoard $ boardSize boardState) ns
                              return onlyOnboard

getStones :: [GoPoint] -> State BoardState [ GoStone]
getStones points = do boardState <- get
                      let stones = map (board boardState M.!) points
                      return stones

neighborsFriends :: GoPoint -> State BoardState [GoPoint]
neighborsFriends p = do boardState <- get
                        ns <- neighbors p
                        stones <- getStones ns
                        let pointStone = fromJust $ M.lookup p (board boardState)
                            pairs = zip ns stones
                            friends = filter (\(p, s) -> s == pointStone) pairs
                        return (map fst friends)


neighborsEnemies :: GoPoint -> State BoardState [GoPoint]
neighborsEnemies p = do boardState <- get
                        ns <- neighbors p
                        stones <- getStones ns
                        let pointStone = fromJust $ M.lookup p (board boardState)
                            pairs = zip ns stones
                            enemies = filter (\(p, s) -> s == opposite pointStone) pairs
                        return (map fst enemies)

neighborsLiberties :: GoPoint -> State BoardState [GoPoint]
neighborsLiberties p = do boardState <- get
                          ns <- neighbors p
                          stones <- getStones ns
                          let pairs = zip ns stones
                              enemies = filter (\(p, s) -> s == Empty) pairs
                          return (map fst enemies)


-- given boardState and a point, 
-- return the list of points 
-- connected by same-colored stones
findDragon ::  S.Set GoPoint ->  GoPoint -> State BoardState [GoPoint]
findDragon acc point = do nfriends <- neighborsFriends point
                          let newFriends = filter (`S.member` acc) nfriends
                              newAcc = S.union acc (S.fromList nfriends)
                          newPoints <- mapM (findDragon (S.insert point newAcc)) newFriends
                          return (point : concat newPoints)

-- given boardState and a point,
-- find all adjacent enemy dragons
findAdjacentEnemyDragons :: GoPoint -> State BoardState [[GoPoint]]
findAdjacentEnemyDragons  point = do boardState <- get
                                     return []

calcLiberties ::  GoPoint -> State BoardState Int
calcLiberties point = do ns <- neighbors point
                         return 0

placeStone :: GoPoint -> GoStone -> State BoardState ()
placeStone point stone = do boardState <- get
                            put boardState{board = M.insert point stone (board boardState)}