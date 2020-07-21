{-# OPTIONS_GHC -Wall -Wno-name-shadowing -fwarn-tabs #-}
module Minimax where

import Board

-------------------------------------------------------------------
-- Tree that stores all possible board states with its terminal
-- and utility value, with the subtrees of all possible "next" moves
-- (including invalid moves) and indexed by position

data Tree = Node Board Player Bool Util [(Position, Tree)]

-- Generates the tree given the board size and starting player
makeTree :: Int -> Player -> Tree
makeTree n p
  = makeTree' empty p
    where
      size  = n * n
      empty = (replicate size Empty, n)
      makeTree' :: Board -> Player -> Tree
      makeTree' b@(cells, _) p'
        = Node b p' term util (map makeChildTree (actions b))
          where
            (term, util)  = terminalUtility b (player p')
            makeChildTree :: Position -> (Position, Tree)
            makeChildTree pos
              = (pos, makeTree' (replace pos (Taken p') cells, n) (player p'))

-------------------------------------------------------------------
-- Helper function to access the elements in the tree

treeBoard :: Tree -> Board
treeBoard (Node b _ _ _ _)
  = b

treePlayer :: Tree -> Player
treePlayer (Node _ p _ _ _)
  = p

treeTerminal :: Tree -> Bool
treeTerminal (Node _ _ term _ _)
  = term

treeUtility :: Tree -> Util
treeUtility (Node _ _ _ util _)
  = util

treeChildren :: Tree -> [Tree]
treeChildren (Node _ _ _ _ ts)
  = [t | (_, t) <- ts]

-------------------------------------------------------------------
-- Helpful type definitions and constants

type Util = Int
type Level = Int
type AgentState = (Tree, Util)

-------------------------------------------------------------------
-- Minimax agent helper functions

-- Returns a list of all possible actions that can be taken given
-- the current state of a board
actions :: Board -> [Position]
actions (cells, _)
  = [i | (c, i) <- zip cells [0..], c == Empty]

validMove :: Position -> Board -> Maybe Position
validMove pos (cells, n)
  | pos < 0 || (n * n) <= pos = Nothing
  | cells !! pos /= Empty     = Nothing
  | otherwise                 = Just pos

result :: Player -> Position -> Board -> Maybe Board
result p pos b@(cells, n)
  = do
      pos' <- validMove pos b
      let cells' = replace pos' (Taken p) cells
      return (cells', n)

-- Helper method for calculating terminal and utility
-- Created to calculate util only once per terminal check
terminalUtility :: Board -> Player -> (Bool, Util)
terminalUtility b@(cells, n) p
  = (isFull cells || abs utilP == n || abs utilP' == n, utilP)
    where
      utilP   = util p
      utilP'  = util (player p)
      isFull :: [Cell] -> Bool
      isFull
        = not . (elem Empty)
      util :: Player -> Util
      util p'
        = find $ map (util' 0) $ concat [f b | f <- [rows, cols, diags]]
          where
            (find, op)
              = case p' of  X -> (maximum, (+))
                            O -> (minimum, (-))
            opp' :: Player
            opp'
              = player p'
            util' :: Util -> [Cell] -> Util
            util' u []
              = u
            util' u ((Taken p'') : cs)
              | p'' == opp' = 0
              | otherwise     = util' (u `op` 1) cs
            util' u (Empty : cs)
              = util' u cs

-- Returns true if game is over, false otherwise
terminal :: Board -> Player -> Bool
terminal
  = (fst .) . terminalUtility

-- Given a player, returns the maximum number of uninterrupted squares
-- X max player (value +ve) and O min player (value -ve)
utility :: Board -> Player -> Util
--Pre: b must be in a terminal state
utility
  = (snd .) . terminalUtility

-- Get the maximum/minimum bound for utility depending on player
utilBound :: Player -> Int -> Util
utilBound X n
  = (n + 1)
utilBound O n
  = -(n + 1)

-------------------------------------------------------------------
-- Minimax agent

-- Returns the subtree after the minimax AI agent decided on its best move
minimax :: Tree -> Level -> Tree
minimax t lvlMax
--Pre: lvlMax > 0
  = t'
    where
      (_, n)  = treeBoard t
      (t', _) = minimax' t (utilBound (treePlayer t) n) 0
      minimax' :: Tree -> Util -> Level -> AgentState
      minimax' t prevBound lvl
        -- Depth-limit search
        | lvl >= lvlMax   = (t, treeUtility t)
        | treeTerminal t  = (t, treeUtility t)
        | otherwise       = bestBoard (treeChildren t) unitState prevBound
          where
            p = treePlayer t
            (comp, comp') = case p of X -> ((>), min)
                                      O -> ((<), max)
            unitState :: AgentState
            unitState
              = (t, utilBound (player p) n)
            bestBoard :: [Tree] -> AgentState -> Util -> AgentState
            bestBoard [] as _
              = as
            bestBoard (t1 : ts) as@(_, u) prevBound
              = bestBoard ts newState (comp' prevBound u')
                where
                  (_, u') = minimax' t1 prevBound (lvl + 1)
                  newState
                    | u `comp` prevBound  = as
                    | u' `comp` u         = (t1, u')
                    | otherwise           = as

            {-
            unitState :: Either AgentState AgentState
            unitState
              = Right (t, utilBound (player p) n)
            bestBoard :: [Tree] -> Either AgentState AgentState -> Util -> AgentState
            bestBoard _ (Left as) _
            -- Alpha-Beta pruning
              = as
            bestBoard [] (Right as) _
              = as
            bestBoard (t1 : ts) (Right as@(_, u)) prevBound
              = bestBoard ts newState (comp' prevBound u')
                where
                  (_, u') = minimax' t1 prevBound (lvl + 1)
                  newState
                    -- Alpha-Beta pruning
                    | u `comp` prevBound || u == prevBound  = Left as
                    -- 'next' move is better
                    | u' `comp` u                           = Right (t1, u')
                    -- 'current' move is better
                    | otherwise                             = Right as
-}
