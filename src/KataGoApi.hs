{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module KataGoApi (getScore, scoreAllMoves) where

import Control.Concurrent (forkIO)
import Control.Exception (throwIO)
import Control.Monad (liftM, unless, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON, ToJSON, Value (Number, Object, String), decode, eitherDecode, encode, fromJSON, parseJSON)
import Data.Aeson.Encoding (string)
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString as B
import Data.ByteString.Builder (byteString)
import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Lazy as BLS
import Data.Char (isPunctuation)
import Data.IntMap.Merge.Lazy (mapWhenMatched)
import Data.List (isInfixOf)
import Data.Maybe (fromJust, isJust, mapMaybe)
import qualified Data.Text as T
import Data.Text.Array (run)
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
import Data.Map (Map)
import GHC.Generics (Generic)
import Network.HTTP.Conduit
  ( Request (method, port, requestBody, requestHeaders, responseTimeout, secure),
    RequestBody (RequestBodyBS, RequestBodyLBS),
    Response (responseBody),
    http,
    httpLbs,
    newManager,
    parseRequest,
    responseTimeoutMicro,
    tlsManagerSettings,
  )
import Network.HTTP.Simple (getResponseBody, httpJSON, httpLBS, setRequestBodyJSON, setRequestHeaders, setRequestMethod, setRequestPort, setRequestResponseTimeout)
import Network.HTTP.Types
import Goban (BoardState(board), GoStone, differences)
import Kifu (boardLetters)
import qualified Data.Map as M


data KataGoRequest = KataGoRequest
  {
    board_size :: Integer,
    moves::[String],
    rules::String,
    ownership :: String,
    komi :: Double
  }
  deriving (Generic, Show)

data KataGoDiagnostics = KataGoDiagnostics {
  best_ten :: Maybe [Map String String],
  bot_move :: Maybe String,
  score::Double,
  winprob::Double
}  deriving (Generic, Show)


data KataGoResponse = KataGoResponse
  { diagnostics :: KataGoDiagnostics,
    probs :: [String],
    request_id :: String
  }  deriving (Generic, Show)

instance ToJSON KataGoRequest
instance FromJSON KataGoDiagnostics
instance FromJSON KataGoResponse

parseKataGoResponse :: BLS.ByteString -> Either String KataGoResponse
parseKataGoResponse = eitherDecode

parseScore :: BLS.ByteString -> Either String Double
parseScore rsp = let srsp = parseKataGoResponse rsp
                  in fmap (score . diagnostics) srsp

makeBoardPoint :: Integer -> Integer -> Integer -> String
makeBoardPoint boardSize x y = (boardLetters !! fromInteger x) ++ show (fromInteger boardSize - y)

moveNameLookup :: Integer -> M.Map (Integer, Integer) String
moveNameLookup boardSize = let pointTuples = [ ((x,y), makeBoardPoint boardSize x y ) | x <- [0..boardSize], y <- [0..boardSize]]
                            in M.fromList pointTuples


translateMoves :: Integer -> [(GoStone, Integer, Integer, Integer)] -> [String]
translateMoves boardSize = let bp = makeBoardPoint boardSize
                           in map (\(color, x,y, movenumber) -> bp x y)


getScore :: String -> Int -> Integer -> [(GoStone, Integer, Integer, Integer)] -> IO Double
getScore host apiPort boardSize moves =
  let boardMoves = translateMoves boardSize moves
      payload = KataGoRequest {board_size=boardSize, moves=boardMoves, ownership="false", rules="japanese", komi=7.5}
      url = "http://" ++ host ++ "/score/katago_gtp_bot"
   in do
        request' <- parseRequest url
        let request =
              setRequestResponseTimeout (responseTimeoutMicro (500 * 1000000))
                . setRequestHeaders [(hContentType, "application/json")]
                . setRequestBodyJSON payload
                . setRequestMethod "POST"
                . setRequestPort apiPort
                $ request'

        rsp <- httpLBS request
        let d = parseScore $ getResponseBody rsp
        case d of
          Left err -> liftIO $ print ("Error parsing result from katago API: " ++ err) >> return 0.0
          Right dur -> return dur


moveList :: [i] -> [[i]]
moveList is = let takelist = [1..length is]
               in map (`take` is) takelist



scoreAllMoves :: String -> Int -> Integer -> [(GoStone, Integer, Integer, Integer)] -> IO [Double]
scoreAllMoves host apiPort boardSize moves = let scoreGetter = getScore host apiPort boardSize
                                              in do rawscores <- mapM scoreGetter (moveList moves)
                                                    let realscore = differences rawscores
                                                        numbered =  zip [1..] realscore
                                                        badmoves = filter (\(m,s) -> s < (-1.0)) numbered
                                                    return realscore
