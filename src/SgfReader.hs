{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module SgfReader where

import           Data.ByteString             (ByteString, pack,
                                              unpack)
import qualified Data.ByteString.UTF8        as BSU
import           Data.List                   hiding ((!!))
import           Data.SGF
import           Data.Tree
import           Kifu
import           Prelude                     hiding (getContents, (!!))
import Data.Maybe ( mapMaybe, fromMaybe, fromJust )
import System.IO (getContents)


import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.SGF as SGF

(!!) = genericIndex

grabGameInfo :: [Word8] -> Maybe (Integer, Integer)
grabGameInfo s = case runParser collection () "stdin" s of
  Right ([Game {tree = TreeGo t, size =sz}], _) -> sz

grabTree :: [Word8] -> TreeGo
grabTree s = case runParser collection () "stdin" s of
  Right ([Game {tree = TreeGo t}], _) -> t

pointToPlay :: Point -> MoveGo
pointToPlay = Play

grabWhoseTurn :: TreeGo  -> [Maybe Color]
grabWhoseTurn  n = [whoseTurn| Left Setup {addBlack = bmoves, addWhite=wmoves, remove = removePoints, toPlay=whoseTurn} <- mainLine]    where
      mainLine = map action . head . transpose . levels $ n

grabBlackSetup :: TreeGo -> [MoveGo]
grabBlackSetup
  n = concat [map pointToPlay (Set.toList bmoves) | Left Setup {addBlack = bmoves, addWhite=wmoves, remove = removePoints, toPlay=whoseTurn} <- mainLine]    where
      mainLine = map action . head . transpose . levels $ n

grabWhiteSetup :: TreeGo -> [MoveGo]
grabWhiteSetup
  n = concat [map pointToPlay (Set.toList wmoves) | Left Setup {addBlack = bmoves, addWhite=wmoves, remove = removePoints, toPlay=whoseTurn} <- mainLine]    where
      mainLine = map action . head . transpose . levels $ n


grabMoves :: TreeGo -> [(Color,MoveGo)]
grabMoves
  n = [(color,move) | Right Move {move = Just (color, move)} <- mainLine]
    where
      mainLine = map action . head . transpose . levels $ n

parse :: ByteString -> [(Color,MoveGo)]
parse = grabMoves . grabTree . unpack

parseGameInfo :: ByteString -> Maybe (Integer, Integer)
parseGameInfo= grabGameInfo  . unpack

parseBlackSetupMoves :: ByteString -> [MoveGo]
parseBlackSetupMoves = grabBlackSetup. grabTree. unpack

parseWhiteSetupMoves :: ByteString -> [MoveGo]
parseWhiteSetupMoves = grabWhiteSetup . grabTree . unpack

coordinates :: [Char]
coordinates = delete 'I' ['A' .. 'Z']

-- showPoint :: Point -> String
-- showPoint (x, y) = coordinates !! x : show (19 - y)

showPoint :: Point -> String
showPoint (x, y) = show x ++ "," ++ show y--coordinates !! x : show (19 - y)


pad :: String -> String
pad s = s ++ replicate (4 - length s) ' '

showMove :: MoveGo -> String
showMove Pass     = "pass"
showMove (Play p) = pad (showPoint p)

showMoves :: [MoveGo] -> String
showMoves = unlines . showMoves' 1
  where
    showMoves' n [] = []
    showMoves' n [m] = [unwords [show n ++ ".", showMove m]]
    showMoves' n (m : m' : ms) = unwords [show n ++ ".", showMove m, showMove m'] : showMoves' (n + 2) ms

getCoords :: MoveGo -> Maybe (Integer,Integer)
getCoords move = case move of
  Pass -> Nothing
  Play p -> Just p

addColors :: [m] -> [(Color, m)]
addColors moves = let moveCount = length moves
                      colors = take moveCount $ concat $ replicate moveCount [Black, White]
                   in zip colors moves


readSgf ::  FilePath -> IO (Integer, [(Color,MoveGo)])
readSgf path = do
  rawSgfData <- readFile path
  let sgfData = BSU.fromString rawSgfData
  let sz = fst $ fromMaybe  (19,19) $ parseGameInfo sgfData
  let gameMoves = parse sgfData
  let blackSetup = map ((,) SGF.Black) (parseBlackSetupMoves sgfData)
  let whiteSetup = map ((,) SGF.White) (parseWhiteSetupMoves sgfData)
  let moves = blackSetup ++ whiteSetup ++ gameMoves
  return (sz, moves)


