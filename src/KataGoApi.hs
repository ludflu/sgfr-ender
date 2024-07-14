{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module KataGoApi (getScore) where

-- import Conduit (ConduitM, ConduitT, MonadResource, awaitForever, concatC, concatMapAccumC, concatMapC, concatMapCE, filterC, leftover, mapAccumWhileC, mapC, mapCE, mapM_C, runConduit, runConduitRes, sinkLazy, sourceLazy, yield, (.|))
import Control.Concurrent (forkIO)
-- import Control.Concurrent.STM (STM, TQueue, atomically, readTVar, writeTQueue, writeTVar)
import Control.Exception (throwIO)
import Control.Monad (liftM, unless, when)
import Control.Monad.IO.Class (MonadIO (..))
-- import Control.Monad.Trans.Resource (ResourceT, liftResourceT, runResourceT)
import Data.Aeson (FromJSON, ToJSON, Value (Number, Object, String), decode, eitherDecode, encode, fromJSON, parseJSON)
import Data.Aeson.Encoding (string)
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString as B
import Data.ByteString.Builder (byteString)
import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Lazy as BLS
import Data.Char (isPunctuation)
-- import Data.Conduit.Binary (sinkFile, sinkHandle, sinkLbs, sourceLbs)
-- import Data.Conduit.Combinators (concatMapE, concatMapM, mapAccumWhile, mapE, splitOnUnboundedE)
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
import Goban (BoardState(board))

data KataGoRequest = KataGoRequest
  { board_size :: Integer,
    moves::[String]
  }
  deriving (Generic, Show)

data KataGoDiagnostics = KataGoDiagnostics {
  best_ten :: [Map String String],
  bot_move :: String,
  score::Double,
  winprob::Double
}  deriving (Generic, Show)


data KataGoResponse = KataGoResponse
  { diagnostics :: String,
    probs :: [Double],
    request_id :: String
  }  deriving (Generic, Show)

instance ToJSON KataGoRequest
instance FromJSON KataGoDiagnostics
instance FromJSON KataGoResponse

parseScore :: BLS.ByteString -> Either String Double
parseScore rsp = let srsp = eitherDecode rsp
                  in fmap score srsp

getScore :: String -> Int -> [String] -> IO Double
getScore host apiPort moves =
  let payload = KataGoRequest {board_size=19, moves= ["A10"]}
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
         in case d of
              Left err -> liftIO $ print ("Error parsing result from katago API: " ++ err) >> return 0.0
              Right dur -> return dur
