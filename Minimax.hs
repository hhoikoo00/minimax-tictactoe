{-# OPTIONS_GHC -Wall -Wno-name-shadowing -fwarn-tabs #-}
module Minimax where

import Data.Maybe

import Board

-------------------------------------------------------------------
-- Minimax agent helper functions

-- Returns a list of all possible actions that can be taken given
-- the current state of a board
actions :: Board -> [Position]
actions (cells, _)
  = [i | (c, i) <- zip cells [0..], c == Empty]

-- Returns the next player in turn
player :: Player -> Player
player X  = O
player O  = X

-- Only moves that are in the board and the position is Empty
-- are accepted by the function
result :: Player -> Position -> Board -> Maybe Board
result p pos (cells, n)
  | pos < 0 || (n * n) <= pos = Nothing
  | cells !! pos /= Empty     = Nothing
  | otherwise                 = Just (cells', n)
    where
      cells'  = replace pos (Taken p) cells

-- Helper method for calculating terminal and utility
-- Created to calculate util only once per terminal check
terminalUtility :: Board -> Player -> (Bool, Int)
terminalUtility b@(cells, n) p
  = (isFull cells || abs util == n || abs util' == n, util)
    where
      util  = utility b p
      util' = utility b (player p)
      isFull :: [Cell] -> Bool
      isFull
        = not . (elem Empty)

-- Returns true if game is over, false otherwise
terminal :: Board -> Player -> Bool
terminal
  = (fst .) . terminalUtility

-- Given a player, returns the maximum number of uninterrupted squares
-- X max player (so +ve) and O min player (so -ve)
utility :: Board -> Player -> Int
--Pre: b must be in a terminal state
utility b p
  = find $ map (flip utility' 0) $ concat [f b | f <- [rows, cols, diags]]
    where
      opponent  = player p
      find :: [Int] -> Int
      find  = case p of X -> maximum
                        O -> minimum
      op :: Int -> Int -> Int
      op    = case p of X -> (+)
                        O -> (-)
      utility' :: [Cell] -> Int -> Int
      utility' [] u
        = u
      utility' ((Taken p) : cs) u
        | p == opponent = 0
        | otherwise     = utility' cs (u `op` 1)
      utility' (Empty : cs) u
        = utility' cs u

-- Get the maximum/minimum bound for utility depending on player
getBound :: Player -> Int -> Int
getBound X n
  = n + 2
getBound O n
  = -(n + 2)

-------------------------------------------------------------------
-- Minimax agent

-- Helpful type definitions and constants
type AgentState = (Int, Board)
type Level = Int

-- Returns the board after the minimax AI agent decided on its best move
minimax :: Player -> Board -> Level -> Board
minimax p b@(_, n) lvlMax
  = b'
    where
      f       = case p of X -> maxValue
                          O -> minValue
      bound   = getBound p n
      (_, b') = f b bound 0 lvlMax

-- Calculation for a Max agent
maxValue :: Board -> Int -> Level -> Level -> AgentState
maxValue
  = getValue True X

-- Calculation for a Min agent
minValue :: Board -> Int -> Level -> Level -> AgentState
minValue
  = getValue False O

-- Helper function for maxValue and minValue (abstracting out the
-- common algorithm)
-- It recursively calculates for all possible actions what would be
-- the best action to take
getValue :: Bool -> Player -> Board -> Int -> Level -> Level -> AgentState
getValue isMax p b@(_, n) prevBound lvl lvlMax
  | lvl > lvlMax || terminate = (util, b)
  | otherwise                 = bestBoard nextBoards (Right (utilBound, b))
    where
      utilBound
        = getBound (player p) n
      (nextGetValue, comp)
        | isMax     = (minValue, (>))   -- Max agent (X)
        | otherwise = (maxValue, (<))   -- Min agent (O)
      (terminate, util)
        = terminalUtility b p
      nextBoards
        = [fromJust (result p a b) | a <- actions b]
      compareBoard :: Board -> AgentState -> Either AgentState AgentState
      compareBoard b as@(u, _)
        | u `comp` prevBound || u == prevBound  = Left as       -- Alpha-beta pruning
        | u' `comp` u         = Right (u', b)
        | otherwise           = Right as
          where
            (u', _) = nextGetValue b u (lvl + 1) lvlMax
      bestBoard :: [Board] -> Either AgentState AgentState -> AgentState
      bestBoard _ (Left as)
        = as
      bestBoard [] (Right as)
        = as
      bestBoard (b : bs) (Right as)
        = bestBoard bs (compareBoard b as)
