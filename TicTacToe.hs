{-# OPTIONS_GHC -Wall -Wno-name-shadowing -fwarn-tabs #-}
module TicTacToe where

import Data.Char
import Data.List
import Text.Read
import System.IO

import Board
import Minimax

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
                          putStrFlush errorMessage
                          doParseAction f errorMessage

-------------------------------------------------------------------
-- I/O Helper Functions

-- Flush the output buffer after printing without newline char
-- to fix the behaviour when program compiled
putStrFlush :: String -> IO ()
putStrFlush text = do
    putStr text
    hFlush stdout

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

-------------------------------------------------------------------
-- Main TicTacToe Game

-- Repeatedly read a target board position and invoke result until
-- the move is successful (Just ...).
takeTurn :: Board -> Player -> Int -> IO Board
takeTurn b p maxlvl
  = do
      if (p == X) then do
        -- If human player; X is human player
        putStrFlush ("Player " ++ (show p) ++ " make your move (row col): ")
        doParseAction userInput "Invalid move, try again: " :: IO Board
      else do
        -- If AI's turn
        putStrLn ("AI is thinking...")
        minimax p b maxlvl
    where
      userInput cin
        = do
            pos <- parsePosition cin
            result p pos b

-- Manage a game by repeatedly: 1. printing the current board, 2. using
-- takeTurn to return a modified board, 3. checking if the game is over,
-- printing the board and a suitable congratulatory message to the winner
-- if so.
playGame :: Board -> Player -> Int -> IO ()
playGame b p maxlvl
  = do
      prettyPrint b
      b' <- takeTurn b p maxlvl
      isGameOver <- return (terminal b')
      if isGameOver then do
        prettyPrint b'
        if (utility b' == 0) then do
          putStrLn ("Draw: No player won.")
        else do
          putStrLn ("Player " ++ (show p) ++ " has won!")
      else if (p == O) then do
        playGame b' X maxlvl
      else do
        playGame b' O maxlvl

-- Print a welcome message, read the board dimension, invoke playGame and
-- exit with a suitable message.
main :: IO ()
main
  = do
      putStrLn "Welcome to tic tac toe on an N x N board"
      putStrFlush "Enter the board size (N > 2): "
      n <- doParseAction getN "Invalid N size, try again: "
      let b = (replicate (n * n) Empty, n)
      putStrFlush "Which player should go first? X (you) or O (AI)?: "
      p <- doParseAction getFirstPlayer "Please select either X or O: "
      putStrFlush "Enter the maximum level to search: "
      maxlvl <- doParseAction getMaxlvl "Level must be greater than 0: "
      playGame b p maxlvl
      putStrLn "Thank you for playing"
    where
      getN str
        = filterMaybe (\i -> i > 2) (readMaybe str :: Maybe Int)
      getFirstPlayer str
        = (readMaybe (map toUpper str) :: Maybe Player)
      getMaxlvl str
        = filterMaybe (\i -> i > 0) (readMaybe str :: Maybe Int)
