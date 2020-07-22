{-# OPTIONS_GHC -Wall -Wno-name-shadowing -fwarn-tabs #-}
module Board where

import Data.List

-------------------------------------------------------------------
data Player = O | X
            deriving (Eq, Show, Read)

data Cell = Empty | Taken Player
          deriving (Eq)

instance Show Cell where
  show Empty      = "-"
  show (Taken p)  = show p

type Board = ([Cell], Int)

type Position = Int

-------------------------------------------------------------------
-- Utility functions

-- Returns the next player in turn
player :: Player -> Player
player X = O
player O = X

-- Preserves Just x iff x satisfies the given predicate. In all other cases
-- (including Nothing) it returns Nothing.
filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe p m@(Just x)
  | p x = m
filterMaybe _ _
  = Nothing

-- Replace nth element of a list with a given item.
replace :: Int -> a -> [a] -> [a]
replace n p ls
  = before ++ (p : after)
    where
      (before, (_ : after)) = splitAt n ls

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
