-- module KifuTests where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Goban
    ( BoardState (moveNumberMap),
      GoPoint(GoPoint),
      GoStone(Black, Empty),
      emptyBoard,
      getStone,
      findDragon,
      libertyCount,
      playBlack,
      playWhite, getAllStones )
import Control.Monad.State
import Diagrams (place)
import Test.Tasty.Providers (IsTest(run))
import Codec.Binary.UTF8.Generic (UTF8Bytes(empty))

main :: IO ()
main = defaultMain allTests

makeDragon :: State BoardState [GoPoint]
makeDragon = do playBlack 5 5 $ Just 1
                playBlack  6 5 $ Just 2
                playBlack  7 5 $ Just 3
                playBlack  8 5 $ Just 4
                findDragon (GoPoint 5 5)

stoneLiberties :: State BoardState Int
stoneLiberties = do playBlack  5 5 $ Just 1
                    libertyCount (GoPoint 5 5)


lowerLeft :: State BoardState GoStone
lowerLeft = do playBlack 15 3 $ Just 1
               getStone $ GoPoint 15 3


lowerLeft2 :: State BoardState [(GoPoint,GoStone)]
lowerLeft2 = do
                playBlack 15 3 $ Just 1
                gets getAllStones

pairStoneLiberties :: State BoardState Int
pairStoneLiberties = do playBlack  5 5 $ Just 1
                        playBlack  5 6 $ Just 2
                        libertyCount (GoPoint 5 5)


atariStone :: State BoardState GoStone
atariStone = do playBlack  5 5 $ Just 1
                playWhite  4 5 $ Just 2
                playWhite  6 5 $ Just 3
                playWhite  5 4 $ Just 4
                playWhite  5 6 $ Just 5
                getStone (GoPoint 5 5)

cornerStone = do playBlack  0 0 $ Just 1
                 libertyCount (GoPoint 0 0)

sideStone = do playBlack  5 0 $ Just 1
               libertyCount (GoPoint 5 0)

allTests :: TestTree
allTests = testGroup "all tests" [testCalcBoundary]

iboard = emptyBoard 19

testCalcBoundary :: TestTree
testCalcBoundary =
  testGroup
    "goban tests" [

        testCase "we should find a dragon" $ evalState makeDragon iboard  @?=
            [GoPoint 5 5, GoPoint 6 5, GoPoint 7 5, GoPoint 8 5],

        testCase "a lone stone should have 4 liberties" $
            evalState stoneLiberties iboard @?= 4,

        testCase "two stones should have 6 liberties" $
            evalState pairStoneLiberties iboard @?= 6,

        testCase "a stone in atari should have zero liberties, get captured" $
            evalState atariStone iboard @?= Empty,

        testCase "a stone in the corner should have 2 liberties" $
            evalState cornerStone iboard @?= 2,

        testCase "a side stone should have 3 liberties" $
            evalState sideStone iboard @?= 3,

        testCase "playing a stone should put it on the board" $
            evalState lowerLeft iboard @?= Black,

        testCase "playing a stone should put it on the board" $
            evalState lowerLeft2 iboard @?= [(GoPoint 15 3, Black)]

        -- testCase "rendering one stone play should get a diagram" $ do
        --     r <- renderOneStone 
        --     r @?= ()
            
    ]