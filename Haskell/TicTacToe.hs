{-# OPTIONS_GHC -Wall -Wno-name-shadowing -fwarn-tabs #-}
module TicTacToe where

import Data.Char
import Data.List
import Text.Read
import System.IO
import Data.Maybe

import Board
import Minimax

-------------------------------------------------------------------
-- Parsing Helper Functions

lookUp :: Eq a => a -> [(a, b)] -> b
lookUp
  = (fromJust .) . lookup

-- Abstraction of 'try action until successful' pattern
doParseAction :: (String -> Maybe a) -> String -> IO a
doParseAction f errorMsg
  = do
      str <- getLine
      let parsed = f str
      case parsed of  (Just val) -> return val
                      Nothing    -> do
                                      putStrFlush errorMsg
                                      doParseAction f errorMsg

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
takeTurn :: Tree -> Int -> IO Tree
takeTurn t maxlvl
  = do
      let p = treePlayer t
      if (p == X) then do
        -- If human player; X is human player
        putStrFlush ("Player " ++ (show p) ++ " make your move (row col): ")
        pos <- doParseAction userInput "Invalid move, try again: "
        let (Node _ _ _ _ children) = t
        return (lookUp pos children)
      else do
        -- If AI's turn
        putStrLn ("AI is thinking...")
        return (minimax t maxlvl)
    where
      b@(_, n)  = treeBoard t
      -- Moves must be of the form "row col" where row and col are integers
      -- separated by whitespace.
      userInput :: String -> Maybe Position
      userInput str
        = do
            [c, c'] <- return (words str)
            x <- readMaybe c :: Maybe Int
            y <- readMaybe c' :: Maybe Int
            validMove (x * n + y) b

-- Manage a game by repeatedly: 1. printing the current board, 2. using
-- takeTurn to return a new subtree, 3. checking if the game is over, printing
-- the board and a suitable congratulatory message to the winner if so.
playGame :: Tree -> Int -> IO ()
playGame t lvlMax
  = do
      prettyPrint (treeBoard t)
      t' <- takeTurn t lvlMax
      if (treeTerminal t') then do
        let b'@(_, n) = treeBoard t'
        prettyPrint b'
        if (abs (treeUtility t') /= n) then do
          putStrLn ("Draw: No player won.")
        else do
          let p = treePlayer t'
          putStrLn ("Player " ++ (show (player p)) ++ " has won!")
      else
        playGame t' lvlMax

-- Print a welcome message, read the board dimension, invoke playGame and
-- exit with a suitable message.
main :: IO ()
main
  = do
      putStrLn "Welcome to tic tac toe on an N x N board"
      -- Get board size
      putStrFlush "Enter the board size (N > 2): "
      n <- doParseAction getN "Invalid N size, try again: "
      -- Get First Player
      putStrFlush "Which player should go first? X (you) or O (AI)?: "
      p <- doParseAction getFirstPlayer "Please select either X or O: "
      -- Generate the tree
      let t = makeTree n p
      -- Get the depth for depth-limit search
      putStrFlush "Enter the maximum level to search: "
      lvlMax <- doParseAction getMaxlvl "Level must be greater than 0: "
      -- Play the game
      playGame t lvlMax
      putStrLn "Thank you for playing"
    where
      getN str
        = filterMaybe (\i -> i > 2) (readMaybe str :: Maybe Int)
      getFirstPlayer str
        = (readMaybe (map toUpper str) :: Maybe Player)
      getMaxlvl str
        = filterMaybe (\i -> i > 0) (readMaybe str :: Maybe Int)
