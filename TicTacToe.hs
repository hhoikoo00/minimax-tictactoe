module TicTacToe where

import Data.Char
import Data.Maybe
import Data.List
import Text.Read

-------------------------------------------------------------------
data Player = O | X
            deriving (Eq, Show)

data Cell = Empty | Taken Player
          deriving (Eq)

instance Show Cell where
  show Empty      = "-"
  show (Taken p)  = show p

type Board = ([Cell], Int)

type Position = (Int, Int)

-------------------------------------------------------------------

--
-- Some useful functions from, or based on, the unassessed problem sheets...
--

-- Preserves Just x iff x satisfies the given predicate. In all other cases
-- (including Nothing) it returns Nothing.
filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe p m@(Just x)
  | p x = m
filterMaybe p _
  = Nothing

-- Replace nth element of a list with a given item.
replace :: Int -> a -> [a] -> [a]
replace 0 p (c : cs)
  = p : cs
replace _ p []
  = []
replace n p (c : cs)
  = c : replace (n - 1) p cs

-- Returns the rows of a given board.
rows :: Board -> [[Cell]]
rows (cs , n)
  = rows' cs
  where
    rows' []
      = []
    rows' cs
      = r : rows' rs
      where
        (r, rs) = splitAt n cs

-- Returns the columns of a given board.
cols :: Board -> [[Cell]]
cols
  = transpose . rows

-- Returns the diagonals of a given board.
diags :: Board -> [[Cell]]
diags (cs, n)
  = map (map (cs !!)) [[k * (n + 1) | k <- [0 .. n - 1]],
                      [k * (n - 1) | k <- [1 .. n]]]

-------------------------------------------------------------------

gameOver :: Board -> Bool
gameOver b@(cells, _)
  = isFull cells || or [gameOver' (f b) | f <- [rows, cols, diags]]
    where
      isFull :: [Cell] -> Bool
      isFull
        = not . (elem Empty)
      gameOver' :: [[Cell]] -> Bool
      gameOver'
        = or . (map isOneElem) . (map nub)
      isOneElem :: [Cell] -> Bool
      isOneElem [Taken _]
        = True
      isOneElem _
        = False

-------------------------------------------------------------------

--
-- Moves must be of the form "row col" where row and col are integers
-- separated by whitespace. Bounds checking happens in tryMove, not here.
--
parsePosition :: String -> Maybe Position
parsePosition str
  = do
      [c, c'] <- return (words str)
      d <- readMaybe c :: Maybe Int
      d' <- readMaybe c' :: Maybe Int
      return (d, d')

tryMove :: Player -> Position -> Board -> Maybe Board
tryMove p (i, j) (cells, n)
  | i < 0 || i >= n || j < 0 || j >= n  = Nothing
  | cells !! pos /= Empty               = Nothing
  | otherwise                           = Just (newCells, n)
    where
      pos       = i * n + j
      newCells  = replace pos (Taken p) cells

-------------------------------------------------------------------
-- I/O Functions

prettyPrint :: Board -> IO ()
prettyPrint b
  = mapM_ prettyPrint' rs
    where
      rs = rows b
      prettyPrint' :: [Cell] -> IO ()
      prettyPrint' cells
        = do
            putStrLn (intersperse ' ' (concatMap show cells))

-- The following reflect the suggested structure, but you can manage the game
-- in any way you see fit.

-- Abstraction of 'try action until successful' pattern
doParseAction :: (String -> Maybe a) -> String -> IO a
doParseAction f errorMessage
  = do
      cin <- getLine
      let parsed = f cin
      case parsed of
        (Just val)  ->  return val
        Nothing     ->  do
                          putStr errorMessage
                          doParseAction f errorMessage

-- Repeatedly read a target board position and invoke tryMove until
-- the move is successful (Just ...).
takeTurn :: Board -> Player -> IO Board
takeTurn b@(cells, _) p
  = do
      putStr ("Player " ++ (show p) ++ " make your move (row col): ")
      doParseAction f "Invalid move, try again: " :: IO Board
    where
      f cin
        = parsePosition cin >>= \pos -> tryMove p pos b

-- Manage a game by repeatedly: 1. printing the current board, 2. using
-- takeTurn to return a modified board, 3. checking if the game is over,
-- printing the board and a suitable congratulatory message to the winner
-- if so.
playGame :: Board -> Player -> IO ()
playGame b@(cells, _) p
  = do
      prettyPrint b
      b' <- takeTurn b p
      isGameOver <- return (gameOver b')
      if isGameOver
        then do putStrLn ("Player " ++ (show p) ++ " has won!")
      else if (p == O)
        then do playGame b' X
      else
        do playGame b' O

-- Print a welcome message, read the board dimension, invoke playGame and
-- exit with a suitable message.
main :: IO ()
main
  = do
      putStrLn "Welcome to tic tac toe on an N x N board"
      putStr "Enter the board size (N): "
      n <- doParseAction f "Invalid N size, try again: "
      let b = (replicate (n * n) Empty, n)
      playGame b O
      putStrLn "Thank you for playing"
    where
      f x
        = filterMaybe (\y -> (0 < y)) (readMaybe x :: Maybe Int)

-------------------------------------------------------------------

testBoard1, testBoard2, testBoard3 :: Board

testBoard1
  = ([Taken O,Taken X,Empty,Taken O,
      Taken O,Empty,Taken X,Taken X,
      Taken O,Empty,Empty,Taken X,
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

testBoard4 :: ([Cell], Int)
testBoard4
  = ([Taken O,Taken X,Taken O,
      Taken X,Taken O,Taken O,
      Taken X,Taken O,Taken X],
      3)
