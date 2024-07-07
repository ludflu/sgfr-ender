-- {-# LANGUAGE FlexibleContexts          #-}
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

data GoStone = Black | White deriving (Eq, Show)

newtype BoardState = BoardState { 
  board :: M.Map (Int,Int) GoStone
} deriving (Show)

