{-# OPTIONS_GHC -Wall -Wno-name-shadowing -fwarn-tabs #-}
module TicTacToe where

import Data.Char
import Data.Maybe
import Data.List
import Text.Read

-------------------------------------------------------------------
data Player = O | X
            deriving (Eq, Show, Read)

data Cell = Empty | Taken Player
          deriving (Eq)

instance Show Cell where
  show Empty      = "-"
  show (Taken p)  = show p

type Board = ([Cell], Int)

type Position = (Int, Int)

-------------------------------------------------------------------
-- Helper utility functions

-- Preserves Just x iff x satisfies the given predicate. In all other cases
-- (including Nothing) it returns Nothing.
filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe p m@(Just x)
  | p x = m
filterMaybe _ _
  = Nothing

-- Replace nth element of a list with a given item.
replace :: Int -> a -> [a] -> [a]
replace 0 p (_ : cs)
  = p : cs
replace _ _ []
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
-- TicTacToe agent helper functions

-- Returns a list of all possible actions that can be taken given
-- the current state of a board
actions :: Board -> [Position]
actions (cells, n)
  = [(i, j) | i <- [0..n-1], j <- [0..n-1], cells !! (i * n + j) == Empty]

-- Returns the next player in turn
player :: Player -> Player
player p
  | p == X    = O
  | otherwise = X

-- Only moves that are in the board and the position is Empty
-- are accepted by the function
result :: Player -> Position -> Board -> Maybe Board
result p (i, j) (cells, n)
  | i < 0 || i >= n || j < 0 || j >= n  = Nothing
  | cells !! pos /= Empty               = Nothing
  | otherwise                           = Just (newCells, n)
    where
      pos       = i * n + j
      newCells  = replace pos (Taken p) cells

-- Returns true if game is over, false otherwise
terminal :: Board -> Bool
terminal b@(cells, _)
  = isFull cells || utility b /= 0
    where
      isFull :: [Cell] -> Bool
      isFull
        = not . (elem Empty)

-- Returns 1 if X won, -1 if O won, 0 otherwise
utility :: Board -> Int
--Pre: b must be in a terminal state
utility b
  = sum [utility' (f b) | f <- [rows, cols, diags]]
    where
      utility' :: [[Cell]] -> Int
      utility'
        = sum . (map isSingleRow) . (map nub)
      isSingleRow :: [Cell] -> Int
      isSingleRow [Taken X]
        = 1
      isSingleRow [Taken O]
        = -1
      isSingleRow _
        = 0

-------------------------------------------------------------------
-- Minimax agent

-- Helpful type definitions and constants
type AgentState = (Int, Board)

utilityMinBound, utilityMaxBound :: Int
utilityMinBound = -2
utilityMaxBound = 2

levelMax :: Int
levelMax = 3

-- Returns the board after the minimax AI agent decided on its best move
minimax :: Player -> Board -> IO Board
minimax p b
  = return b'
    where
      (_, b') = case p of X -> maxValue p b 0 utilityMaxBound
                          O -> minValue p b 0 utilityMinBound

-- Calculation for a Max agent
maxValue :: Player -> Board -> Int -> Int -> AgentState
maxValue
  = getValue True

-- Calculation for a Min agent
minValue :: Player -> Board -> Int -> Int -> AgentState
minValue
  = getValue False

-- Helper function for maxValue and minValue (for abstracting out
-- the common algorithm)
-- It recursively calculates for all possible actions what would be
-- the best action to take
getValue :: Bool -> Player -> Board -> Int -> Int -> AgentState
getValue isMax p b level prev
  | level > levelMax || terminal b  = (utility b, b)
  | otherwise                       = bestBoard
    where
      (recurseValue, comp, initv)
        = case isMax of True  -> (minValue, (>), utilityMinBound)
                        False -> (maxValue, (<), utilityMaxBound)
      f :: Board -> AgentState -> AgentState
      f b' as@(u, _)
        | u `comp` prev = as        -- Alpha-beta pruning
        | u' `comp` u   = (u', b')
        | otherwise     = as
          where
            (u', _) = recurseValue (player p) b' (level + 1) u
      nextBoards  = [fromJust (result p a b) | a <- actions b]
      bestBoard   = foldr f (initv, b) nextBoards


-------------------------------------------------------------------
-- Parsing Helper Functions

-- Moves must be of the form "row col" where row and col are integers
-- separated by whitespace. Bounds checking happens in result, not here.
parsePosition :: String -> Maybe Position
parsePosition str
  = do
      [c, c'] <- return (words str)
      d <- readMaybe c :: Maybe Int
      d' <- readMaybe c' :: Maybe Int
      return (d, d')

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

-------------------------------------------------------------------
-- I/O Functions

-- Prints the given board, players represented by how
-- Player instantiates Show
prettyPrint :: Board -> IO ()
prettyPrint b
  = mapM_ prettyPrint' rs
    where
      rs = rows b
      prettyPrint' :: [Cell] -> IO ()
      prettyPrint' cells
        = do
            putStrLn (intersperse ' ' (concatMap show cells))

-- Repeatedly read a target board position and invoke result until
-- the move is successful (Just ...).
takeTurn :: Board -> Player -> IO Board
takeTurn b p
  = do
      if (p == X) then do
        putStr ("Player " ++ (show p) ++ " make your move (row col): ")
        doParseAction userInput "Invalid move, try again: " :: IO Board
      else do
        putStrLn ("AI is thinking...")
        minimax p b
    where
      userInput cin
        = do
            pos <- parsePosition cin
            result p pos b

-- Manage a game by repeatedly: 1. printing the current board, 2. using
-- takeTurn to return a modified board, 3. checking if the game is over,
-- printing the board and a suitable congratulatory message to the winner
-- if so.
playGame :: Board -> Player -> IO ()
playGame b p
  = do
      prettyPrint b
      b' <- takeTurn b p
      isGameOver <- return (terminal b')
      if isGameOver then do
        prettyPrint b'
        if (utility b' == 0) then do
          putStrLn ("Draw: No player won.")
        else do
          putStrLn ("Player " ++ (show p) ++ " has won!")
      else if (p == O) then do
        playGame b' X
      else do
        playGame b' O

-- Print a welcome message, read the board dimension, invoke playGame and
-- exit with a suitable message.
main :: IO ()
main
  = do
      putStrLn "Welcome to tic tac toe on an N x N board"
      putStr "Enter the board size (N > 2): "
      n <- doParseAction getSize "Invalid N size, try again: "
      let b = (replicate (n * n) Empty, n)
      putStr "Which player should go first? X (you) or O (AI)?: "
      p <- doParseAction getFirstOrder "Please select either X or O: "
      playGame b p
      putStrLn "Thank you for playing"
    where
      getSize str
        = filterMaybe (\i -> i > 2) (readMaybe str :: Maybe Int)
      getFirstOrder str
        = (readMaybe (map toUpper str) :: Maybe Player)
