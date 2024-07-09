
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Goban
import Control.Monad.State
import Diagrams (place)
import Test.Tasty.Providers (IsTest(run))

main :: IO ()
main = defaultMain allTests

makeDragon :: State BoardState [GoPoint]
makeDragon = do placeStone (GoPoint 5 5) Black
                placeStone (GoPoint 6 5) Black
                placeStone (GoPoint 7 5) Black
                placeStone (GoPoint 8 5) Black
                findDragon (GoPoint 5 5)

makeDragonTest = fst $ runState makeDragon emptyBoard
expected =  [GoPoint 5 5, GoPoint 6 5, GoPoint 7 5, GoPoint 8 5]

allTests :: TestTree
allTests = testGroup "all tests" [testCalcBoundary]

testCalcBoundary :: TestTree
testCalcBoundary =
  testGroup
    "goban tests" [ 

        testCase "we should find a dragon" $ makeDragonTest @?= expected
    ]