
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Goban
import Control.Monad.State
import Diagrams (place)
import Test.Tasty.Providers (IsTest(run))
import Codec.Binary.UTF8.Generic (UTF8Bytes(empty))

main :: IO ()
main = defaultMain allTests

makeDragon :: State BoardState [GoPoint]
makeDragon = do placeStone (GoPoint 5 5) Black
                placeStone (GoPoint 6 5) Black
                placeStone (GoPoint 7 5) Black
                placeStone (GoPoint 8 5) Black
                findDragon (GoPoint 5 5)

stoneLiberties :: State BoardState Int
stoneLiberties = do placeStone (GoPoint 5 5) Black
                    libertyCount (GoPoint 5 5)

pairStoneLiberties :: State BoardState Int
pairStoneLiberties = do placeStone (GoPoint 5 5) Black
                        placeStone (GoPoint 5 6) Black
                        libertyCount (GoPoint 5 5)


atariStone :: State BoardState Int
atariStone = do placeStone (GoPoint 5 5) Black
                placeStone (GoPoint 4 5) White
                placeStone (GoPoint 6 5) White
                placeStone (GoPoint 5 4) White
                placeStone (GoPoint 5 6) White
                libertyCount (GoPoint 5 5)

cornerStone = do placeStone (GoPoint 0 0) Black
                 libertyCount (GoPoint 0 0)

sideStone = do placeStone (GoPoint 5 0) Black
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