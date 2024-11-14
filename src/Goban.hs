{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module Goban where

import           Data.ByteString             (ByteString, getContents, pack,
                                              unpack)
import qualified Data.ByteString.UTF8        as BSU
import           Data.List                   hiding ((!!))

import           Data.Tree
import           Prelude                     hiding (getContents, (!!))
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State
import Diagrams (place)
-- import Diagrams.TwoD.Path.LSystem (dragon)

--this module is used to calculate what stones to
-- take off the board when a new stone is placed

data GoStone = Black | White | Empty deriving (Eq, Show)
data GoPoint =  GoPoint { x::Integer, y::Integer }deriving (Eq, Show, Ord)
data BoardState = BoardState {
  board :: M.Map GoPoint GoStone,
  moveNumberMap :: M.Map GoPoint Integer,
  boardSize :: Integer,
  moveNumber :: Int
} deriving (Show)

opposite :: GoStone -> GoStone
opposite Black = White
opposite White = Black
opposite Empty = Empty

emptyBoard :: Integer -> BoardState
emptyBoard boardSize = let points = [GoPoint x y | x <- [0..boardSize], y <- [0..boardSize]]
                in BoardState {
                    board = M.fromList $ zip points (repeat Empty),
                    moveNumberMap = M.empty,
                    moveNumber = 1,
                    boardSize = boardSize }

isOnBoard :: Integer -> GoPoint ->  Bool
isOnBoard bSize (GoPoint x y)  = x >= 0 && x <= bSize && y >= 0 && y <= bSize

neighbors :: GoPoint -> State BoardState [GoPoint]
neighbors  (GoPoint x y) = do boardState <- get
                              let ns = [GoPoint (x+1) y, GoPoint (x-1) y, GoPoint x (y+1), GoPoint x (y-1)]
                                  onlyOnboard = filter (isOnBoard $ fromIntegral $ boardSize boardState) ns
                              return onlyOnboard

getStone :: GoPoint -> State BoardState GoStone
getStone point = do boardState <- get
                    return $ board boardState M.! point

getStones :: [GoPoint] -> State BoardState [GoStone]
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
                              empties = filter (\(p, s) -> s == Empty) pairs
                          return (map fst empties)

findDragon' ::  S.Set GoPoint ->  GoPoint -> State BoardState [GoPoint]
findDragon' acc point = do nfriends <- neighborsFriends point
                           let newFriends = filter (`S.notMember` acc)  nfriends
                               newAcc = S.union acc (S.fromList nfriends)
                           newPoints <- mapM (findDragon' (S.insert point newAcc)) newFriends
                           return (point : concat newPoints)

findDragon :: GoPoint -> State BoardState [GoPoint]
findDragon point = findDragon' (S.singleton point) point

libertyCount :: GoPoint -> State BoardState Int
libertyCount p = do dragon <- findDragon p
                    libs <- mapM neighborsLiberties dragon
                    return $ length $ S.fromList $ concat libs


-- given boardState and a point,
-- find all adjacent enemy dragons
findAdjacentEnemyDragons :: GoPoint -> State BoardState [[GoPoint]]
findAdjacentEnemyDragons  point = do enemyNeighbors <- neighborsEnemies point
                                     mapM findDragon enemyNeighbors


capture' :: [GoPoint] -> State BoardState ()
capture' = mapM_ (\x -> placeStone Empty x Nothing)

capture :: GoPoint -> State BoardState ()
capture point = do dragon <- findDragon point
                   capture' dragon

placeStone :: GoStone -> GoPoint -> Maybe Integer -> State BoardState ()
placeStone stone point moveNumber = do boardState <- get
                                       put boardState{
                                            board = M.insert point stone (board boardState),
                                            --if moveNumber is Nothing, then the stone is not a move, so don't update moveNumberMap otherwise update it
                                            moveNumberMap = case moveNumber of
                                                Nothing -> moveNumberMap boardState
                                                Just n -> M.insert point n (moveNumberMap boardState)

                                            }


isCapturable :: GoPoint -> State BoardState Bool
isCapturable p = do libertyCount <- libertyCount p
                    return $ libertyCount == 0

playStone :: GoStone -> GoPoint -> Maybe Integer -> State BoardState ()
playStone stone point moveNumber = do placeStone stone point moveNumber
                                      enemies <- neighborsEnemies point
                                      captureAble <- filterM isCapturable enemies
                                      mapM_ capture captureAble

playBlack :: Integer -> Integer -> Maybe Integer -> State BoardState ()
playBlack x y = playStone Black (GoPoint x y)

playWhite :: Integer -> Integer -> Maybe Integer -> State BoardState ()
playWhite x y = playStone White (GoPoint x y)

playMoves :: [(GoStone, Integer, Integer,  Integer)] -> State BoardState ()
playMoves = mapM_ (\(s, x, y, moveNumber) -> playStone s (GoPoint x y) (Just moveNumber))

getAllStones :: BoardState -> [(GoPoint, GoStone)]
getAllStones boardState = let boardMap = board boardState
                           in  filter (\ (p,s) -> s /= Empty ) (M.toList boardMap)


takeLengthOf :: [a] -> [b] -> [b]
takeLengthOf = zipWith (flip const)

windows :: Int -> [a] -> [[a]]
windows n xs = takeLengthOf (drop (n-1) xs) (windows' n xs)

windows' :: Int -> [a] -> [[a]]
windows' n = map (take n) . tails

calcMove :: Double -> Double -> Double
calcMove old new | old <0 && new > 0 = old + new

diffs :: [Double] -> [Double]
diffs scores = let w = windows 2 scores
                in map (\[x,y] -> y + x)  w


differences :: [Double] -> [Double]
differences xs = diffs (0:xs)
