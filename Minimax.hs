{-# OPTIONS_GHC -Wall -Wno-name-shadowing -fwarn-tabs #-}
module Minimax where

import Data.Maybe
import Data.List

import Board

-------------------------------------------------------------------
-- Minimax agent helper functions

-- Returns a list of all possible actions that can be taken given
-- the current state of a board
actions :: Board -> [Position]
actions (cells, n)
  = actions' 0 cells []
    where
      actions' :: Int -> [Cell] -> [Position] -> [Position]
      actions' _ [] memo
        = memo
      actions' k (c : cs) memo
        | c == Empty  = actions' (k + 1) cs (divMod k n: memo)
        | otherwise   = actions' (k + 1) cs memo

-- Returns the next player in turn
player :: Player -> Player
player p
  | p == X    = O
  | otherwise = X

-- Only moves that are in the board and the position is Empty
-- are accepted by the function
result :: Player -> Position -> Board -> Maybe Board
result p (i, j) (cells, n)
  | i < 0 || n <= i       = Nothing
  | j < 0 || n <= j       = Nothing
  | cells !! pos /= Empty = Nothing
  | otherwise             = Just (cells', n)
    where
      pos     = i * n + j
      cells'  = replace pos (Taken p) cells

-- Returns true if game is over, false otherwise
terminal :: Board -> Bool
terminal
  = fst . terminalUtility

terminalUtility :: Board -> (Bool, Int)
terminalUtility b@(cells, _)
  = (isFull cells || util /= 0, util)
    where
      util  = utility b
      isFull :: [Cell] -> Bool
      isFull
        = not . (elem Empty)

-- Returns 1 if X won, -1 if O won, 0 otherwise
utility :: Board -> Int
--Pre: b must be in a terminal state
utility b
  = (sum . concat) [utility' (f b) | f <- [rows, cols, diags]]
    where
      utility' :: [[Cell]] -> [Int]
      utility'
        = (map isSingleRow) . (map nub)
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
type Level = Int

utilMin, utilMax :: Int
utilMin = -2
utilMax = 2

-- Returns the board after the minimax AI agent decided on its best move
minimax :: Player -> Board -> Level -> IO Board
minimax p b levelMax
  = return b'
    where
      (_, b') = case p of X -> maxValue p b utilMax 0 levelMax
                          O -> minValue p b utilMin 0 levelMax

-- Calculation for a Max agent
maxValue :: Player -> Board -> Int -> Level -> Level -> AgentState
maxValue
  = getValue True

-- Calculation for a Min agent
minValue :: Player -> Board -> Int -> Level -> Level -> AgentState
minValue
  = getValue False

-- Helper function for maxValue and minValue (for abstracting out
-- the common algorithm)
-- It recursively calculates for all possible actions what would be
-- the best action to take
getValue :: Bool -> Player -> Board -> Int -> Level -> Level -> AgentState
getValue isMax p b prevBound lvl lvlMax
  | lvl > lvlMax || terminate = (util, b)
  | otherwise                 = bestBoard nextBoards (Right (utilBound, b))
    where
      (nextGetValue, comp, utilBound)
        | isMax     = (minValue, (>), utilMin)
        | otherwise = (maxValue, (<), utilMax)
      (terminate, util)
        = terminalUtility b
      nextBoards
        = [fromJust (result p a b) | a <- actions b]
      bestBoard :: [Board] -> Either AgentState AgentState -> AgentState
      bestBoard _ (Left as)
        = as
      bestBoard [] (Right as)
        = as
      bestBoard (b : bs) (Right as)
        = bestBoard bs (compareBoard b as)
      compareBoard :: Board -> AgentState -> Either AgentState AgentState
      compareBoard b as@(u, _)
        | u `comp` prevBound  = Left as       -- Alpha-beta pruning
        | u' `comp` u         = Right (u', b)
        | otherwise           = Right as
          where
            (u', _) = nextGetValue (player p) b u (lvl + 1) lvlMax
