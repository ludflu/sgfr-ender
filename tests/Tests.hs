
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Goban
import Control.Monad.State
import Diagrams (place)
import Test.Tasty.Providers (IsTest(run))
import Codec.Binary.UTF8.Generic (UTF8Bytes(empty))

main :: IO ()
main = defaultMain allTests

playBlack :: Int -> Int -> State BoardState ()
playBlack x y = placeStone Black (GoPoint x y)
playWhite :: Int -> Int -> State BoardState ()
playWhite x y = placeStone White (GoPoint x y)


makeDragon :: State BoardState [GoPoint]
makeDragon = do playBlack 5 5
                playBlack  6 5
                playBlack  7 5
                playBlack  8 5
                findDragon (GoPoint 5 5)

stoneLiberties :: State BoardState Int
stoneLiberties = do playBlack  5 5
                    libertyCount (GoPoint 5 5)

pairStoneLiberties :: State BoardState Int
pairStoneLiberties = do playBlack  5 5
                        playBlack  5 6
                        libertyCount (GoPoint 5 5)


atariStone :: State BoardState Int
atariStone = do playBlack  5 5
                playWhite  4 5
                playWhite  6 5
                playWhite  5 4
                playWhite  5 6
                libertyCount (GoPoint 5 5)

cornerStone = do playBlack  0 0
                 libertyCount (GoPoint 0 0)

sideStone = do playBlack  5 0
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

        testCase "a stone in atari should have zero liberties" $ 
            evalState atariStone iboard @?= 0,

        testCase "a stone in the corner should have 2 liberties" $ 
            evalState cornerStone iboard @?= 2,

        testCase "a side stone should have 3 liberties" $ 
            evalState sideStone iboard @?= 3

    ]