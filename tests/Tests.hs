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


testScoring = [-0.724965,-1.20026,-0.663085,-0.937061,-1.04828,-1.0223,-0.804977,-1.00124,-0.694311,-0.839242,-0.355908,-0.248622,0.4593,0.619391,1.05513,2.57183,2.59755,7.69033,6.34222,6.90555,3.56523,9.09957,5.83229,7.68652,4.15218,8.18696,5.57077,5.2978,2.73907,2.86939,0.514957,0.501476,3.45977e-2,4.74437,3.90132,5.67592,6.5243,8.43933,8.403,12.688,11.2429,17.3038,12.202,20.7263,13.1371,14.9999,13.6226,16.6582,15.5994,17.7919,15.6573,18.1015,15.7527,17.4703,14.7396,17.725,17.433,19.9938,18.1497,24.2347,23.8397,25.2773,24.7945,25.3926,23.8198,30.4194,29.371,30.3837,31.102,33.4029,31.9449,36.7508,33.2242,41.5012,37.487,43.1937,40.1037,45.6625,38.9791,44.9218,39.083,47.6962,44.0186,54.8412,50.3578,54.8137,53.008,56.3976,53.8995,54.8726,52.6126,61.1792,59.7437,61.3136,62.394,60.1707,57.0319,63.022,57.9446,62.0861,60.3026,73.17,62.4352,71.343,61.4432,67.1115,64.0717,70.8912,67.8905,70.3609,67.3189,71.8537,46.7282,44.2219,35.2471,36.4442,26.3346,26.9502,15.5853,29.0068,16.6692,19.7953,19.8048,36.6518,34.9492,35.7958,27.9822,34.794,28.5174,34.241,31.7362,32.3719,19.9986,32.8731,32.8171,34.0392,34.8992,34.1738,35.1468,36.4547,35.2626,35.7409,36.1838,34.9368,35.8569,42.8615,41.2035,50.2651,45.2022,50.3889,46.9461,49.1749,49.1749,49.9901,49.352,51.9434,50.9722,57.1967,52.336,60.6067,55.4304,56.5821,55.5499,55.1013,52.2309,53.9931,55.2936,55.1826,35.5834,34.1651,35.2369,33.6104,18.7615,17.7009,14.3445,20.0137,22.4438,20.7508,16.5878,28.2533,16.8818,25.0517,16.3894,25.5795,16.1288,24.269,16.261,24.6059,15.9278,25.8851,16.2595,15.8941,16.733,25.9016,16.0851,15.9132,16.4618,25.8179,16.7922,26.7722,16.95,26.6026,17.3427,27.0024,17.2059,26.6245,17.1072,26.8267,21.7455,24.042,23.7115,22.9025,20.7152,21.5536,18.3996,22.0138,18.2917,18.7398,18.3657,18.6616,18.2564,18.2304,18.5642,18.5551,18.4266,18.4123,18.8328,23.7554,19.0591,25.4114,18.7516,19.2007,18.9864,25.1622,19.3334,18.8695,18.37,22.346,20.6304,19.0651,21.6414,24.9603,21.7874,19.1233,23.1757,18.7116,19.8887,26.0773,19.7046,26.9522,20.17,19.1494,20.5842,31.4908,20.2483,30.631,20.9152,24.1184,23.6972,31.3302,20.1712,27.8003,13.7978,11.3728,12.3528,11.8322,11.7028,9.57301,2.71029,11.5758,2.77171,11.651,1.70646,2.10794,2.58699,8.14107,2.81641,2.80987,2.18633,2.41418,2.39179,1.93225,2.28311,2.39549,0.36856,0.273073,0.385488,1.79934,0.424082,0.315574,-4.8109e-2,-0.12211,-1.8584,-0.215188,-1.88937,-1.90958,-1.62956,-2.02021,-1.69503,-2.14583,-1.67252,-2.83889,-2.49928,-2.65048,-2.71357]

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