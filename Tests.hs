module Tests (Tests.main) where

import IC.TestSuite

import Board
import Minimax
import TicTacToe hiding (main)


testBoard1, testBoard1', testBoard2, testBoard3, testBoard3', testBoard4, testBoard5 :: Board

testBoard1
  = ([Taken O,Taken X,Empty,Taken O,
      Taken O,Empty,Taken X,Taken X,
      Taken O,Empty,Empty,Taken X,
      Taken O,Taken X,Empty,Empty],
      4)

testBoard1'
  = ([Taken O,Taken X,Empty,Taken O,
      Taken O,Empty,Taken X,Taken X,
      Empty,Empty,Empty,Taken X,
      Taken O,Taken X,Empty,Empty],
      4)

testBoard2
  = ([Taken X,Empty,
      Empty,Empty],
      2)

testBoard3
  = ([Taken O,Taken X,Empty,Taken O,Taken X,
      Taken O,Empty,Taken X,Taken X,Empty,
      Empty,Empty,Taken X,Taken O,Taken O,
      Taken O,Taken X,Empty,Empty,Taken X,
      Taken X,Empty,Taken O,Empty,Empty],
      5)

testBoard3'
  = ([Taken O,Taken X,Empty,Taken O,Taken X,
      Taken O,Empty,Taken X,Taken X,Empty,
      Empty,Empty,Empty,Taken O,Taken O,
      Taken O,Taken X,Empty,Empty,Taken X,
      Taken X,Empty,Taken O,Empty,Empty],
      5)

testBoard4
  = ([Taken O,Taken X,Taken O,
      Taken X,Taken O,Taken O,
      Taken X,Taken O,Taken X],
      3)

testBoard5
  = (replicate (3 * 3) Empty, 3)

-------------------------------------------------------------------

actionsTestCases
  = [
      testBoard1 ==> reverse [(0,2),(1,1),(2,1),(2,2),(3,2),(3,3)]
    , testBoard2 ==> reverse [(0,1),(1,0),(1,1)]
    , testBoard3 ==> reverse [(0,2),(1,1),(1,4),(2,0),(2,1),
                      (3,2),(3,3),(4,1),(4,3),(4,4)]
    , testBoard4 ==> reverse []
    , testBoard5 ==> reverse [(0,0),(0,1),(0,2),(1,0),
                      (1,1),(1,2),(2,0),(2,1),(2,2)]
    ]

resultTestCases
  = [
      (O, (0, 0), testBoard1) ==> (Nothing)
    , (O, (0, 2), testBoard1) ==> (Just ([Taken O,Taken X,Taken O,Taken O,
                                          Taken O,Empty,Taken X,Taken X,
                                          Taken O,Empty,Empty,Taken X,
                                          Taken O,Taken X,Empty,Empty],4))
    , (O, (-1, 0), testBoard5) ==> (Nothing)
    , (O, (0, -1), testBoard5) ==> (Nothing)
    , (O, (3, 3), testBoard5) ==> (Nothing)
    , (X, (0, 0), testBoard2) ==> (Nothing)
    , (X, (1, 1), testBoard5) ==> (Just ([Empty,Empty,Empty,
                                          Empty,Taken X,Empty,
                                          Empty,Empty,Empty],3))
    ,  (X,(0,0),testBoard2) ==> (Nothing)
    , (O,(-1,2),testBoard2) ==> (Nothing)
    , (O,(0,-1),testBoard2) ==> (Nothing)
    , (O,(1,1),testBoard2) ==> (Just ([Taken X,Empty,
                                       Empty,Taken O],2))
    , (O,(3,3),testBoard1) ==> (Just ([Taken O,Taken X,Empty,Taken O,
                                       Taken O,Empty,Taken X,Taken X,
                                       Taken O,Empty,Empty,Taken X,
                                       Taken O,Taken X,Empty,Taken O],4))
    , (O,(2,1),testBoard4) ==> (Nothing)
    ]

terminalTestCases
  = [
      testBoard1 ==> True
    , testBoard2 ==> False
    , testBoard3 ==> True
    , testBoard4 ==> True
    , testBoard5 ==> False
    ]

utilityTestCases
  = [
      testBoard1 ==> (-1)
    , testBoard3 ==> (1)
    , testBoard4 ==> (0)
    ]

parsePositionTestCases
  = [
      ("0 2") ==> (Just (0,2))
    , ("0 -8") ==> (Just (0,-8))
    , ("-4 1") ==> (Just (-4,1))
    , ("0 %1") ==> (Nothing)
    , ("") ==> (Nothing)
    , (" ") ==> (Nothing)
    , ("1 2 3") ==> (Nothing)
    , ("one two") ==> (Nothing)
    , ("1 two") ==> (Nothing)
    , ("1 2.0") ==> (Nothing)
    , (" -2 3   ") ==> (Just (-2, 3))
    , ("2    3") ==> (Just (2, 3))
    ]

-- You can add your own test cases above

allTestCases
  = [
      TestCase  "actions" (actions)
                actionsTestCases
    , TestCase  "result" (uncurry3 result)
                resultTestCases
    , TestCase  "terminal" (terminal)
                terminalTestCases
    , TestCase  "utility" (utility)
                utilityTestCases
    , TestCase  "parsePosition" (parsePosition)
                parsePositionTestCases
    ]

runTests = mapM_ goTest allTestCases

main = runTests
