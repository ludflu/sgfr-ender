{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module SgfReader where

import           Data.ByteString             (ByteString, getContents, pack,
                                              unpack)
import qualified Data.ByteString.UTF8        as BSU
import           Data.List                   hiding ((!!))
import           Data.SGF
import           Data.Tree
import           Grid                        (exampleGrid)
import           Prelude                     hiding (getContents, (!!))
import Data.Maybe

data GoColor = Black | White deriving (Eq, Show)

(!!) = genericIndex


grabTree :: [Word8] -> TreeGo
grabTree s = case runParser collection () "stdin" s of
  Right ([Game {tree = TreeGo t}], _) -> t

grabMoves :: TreeGo -> [MoveGo]
grabMoves
  n = [move | Right Move {move = Just (color, move)} <- mainLine]
    where
      mainLine = map action . head . transpose . levels $ n

parse :: ByteString -> [MoveGo]
parse = grabMoves . grabTree . unpack

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
    showMoves' n [m] = unwords [show n ++ ".", showMove m] : []
    showMoves' n (m : m' : ms) = unwords [show n ++ ".", showMove m, showMove m'] : showMoves' (n + 2) ms

getCoords :: MoveGo -> Maybe (Integer,Integer)
getCoords move = case move of
  Pass -> Nothing
  Play p -> Just p

addColors :: [m] -> [(Color, m)]  
addColors moves = let moveCount = length moves
                      colors = concat $ take moveCount $ repeat [Data.SGF.Black,Data.SGF.White]
                   in zipWith (,) colors moves

renderReady :: [MoveGo] -> [(Color, (Integer,Integer))]
renderReady moves = let moves' = catMaybes $ map getCoords moves
                      in addColors moves'

readSgf :: FilePath -> IO [MoveGo]
readSgf path = do 
  rawSgfData <- readFile path
  let sgfData = BSU.fromString rawSgfData 
  return $ parse sgfData

  
