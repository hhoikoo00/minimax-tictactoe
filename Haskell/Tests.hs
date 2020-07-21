module Tests (Tests.main) where

import IC.TestSuite

import Board
import BoardTree
import Minimax
import TicTacToe hiding (main)


testBoard1, testBoard1', testBoard2, testBoard3, testBoard3' :: Board
testBoard4, testBoard5, testBoard6, testBoard6' :: Board

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

testBoard6
  = ([Empty, Empty, Taken X, Taken X,
      Empty, Taken X, Empty, Empty,
      Empty, Empty, Taken X, Taken O,
      Taken O, Taken X, Taken O, Taken O],
      4)

testBoard6'
  = ([Empty, Empty, Taken X, Taken X,
      Empty, Taken X, Empty, Empty,
      Taken O, Empty, Taken X, Taken O,
      Taken O, Taken X, Taken O, Taken O],
      4)

testTree1 :: Tree

testTree1 = makeTree 3 X

-------------------------------------------------------------------

actionsTestCases
  = [
      testBoard1 ==> [2,5,9,10,14,15]
    , testBoard2 ==> [1,2,3]
    , testBoard3 ==> [2,6,9,10,11,17,18,21,23,24]
    , testBoard4 ==> []
    , testBoard5 ==> [0,1,2,3,4,5,6,7,8]
    ]

resultTestCases
  = [
      (O, 0, testBoard1) ==> (Nothing)
    , (O, 2, testBoard1) ==> (Just ([Taken O,Taken X,Taken O,Taken O,
                                          Taken O,Empty,Taken X,Taken X,
                                          Taken O,Empty,Empty,Taken X,
                                          Taken O,Taken X,Empty,Empty],4))
    , (O, -5, testBoard5) ==> (Nothing)
    , (O, -1, testBoard5) ==> (Nothing)
    , (O, 18, testBoard5) ==> (Nothing)
    , (X, 0, testBoard2) ==> (Nothing)
    , (X, 4, testBoard5) ==> (Just ([Empty,Empty,Empty,
                                          Empty,Taken X,Empty,
                                          Empty,Empty,Empty],3))
    , (X, 0, testBoard2) ==> (Nothing)
    , (O, -1, testBoard2) ==> (Nothing)
    , (O, 3, testBoard2) ==> (Just ([Taken X,Empty,
                                       Empty,Taken O],2))
    , (O, 15, testBoard1) ==> (Just ([Taken O,Taken X,Empty,Taken O,
                                       Taken O,Empty,Taken X,Taken X,
                                       Taken O,Empty,Empty,Taken X,
                                       Taken O,Taken X,Empty,Taken O],4))
    , (O, 7, testBoard4) ==> (Nothing)
    ]

terminalTestCases
  = [
      (testBoard1, X) ==> (True)
    , (testBoard1, O) ==> (True)
    , (testBoard1', X) ==> (False)
    , (testBoard1', O) ==> (False)
    , (testBoard2, X) ==> (False)
    , (testBoard2, O) ==> (False)
    , (testBoard3, X) ==> (True)
    , (testBoard3, O) ==> (True)
    , (testBoard3', X) ==> (False)
    , (testBoard3', O) ==> (False)
    , (testBoard4, X) ==> (True)
    , (testBoard4, O) ==> (True)
    , (testBoard5, X) ==> (False)
    , (testBoard5, O) ==> (False)
    ]

utilityTestCases
  = [
      (testBoard1, X) ==> (2)
    , (testBoard1, O) ==> (-4)
    , (testBoard1', X) ==> (2)
    , (testBoard1', O) ==> (-3)
    , (testBoard2, X) ==> (1)
    , (testBoard2, O) ==> (0)
    , (testBoard3, X) ==> (5)
    , (testBoard3, O) ==> (0)
    , (testBoard3', X) ==> (4)
    , (testBoard3', O) ==> (-2)
    , (testBoard4, X) ==> (0)
    , (testBoard4, O) ==> (0)
    , (testBoard5, X) ==> (0)
    , (testBoard5, O) ==> (0)
    ]

-- You can add your own test cases above

allTestCases
  = [
      TestCase  "actions" (actions)
                actionsTestCases
    , TestCase  "result" (uncurry3 result)
                resultTestCases
    , TestCase  "terminal" (uncurry terminal)
                terminalTestCases
    , TestCase  "utility" (uncurry utility)
                utilityTestCases
    ]

runTests = mapM_ goTest allTestCases

main = runTests
