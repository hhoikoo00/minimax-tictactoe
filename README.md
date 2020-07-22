# minimax-tictactoe

## Introduction

This is a personal peroject that uses Haskell to implement a Minimax agent for a generic N-by-N sized Tic Tac Toe game.

## Disclaimer

The minimax agent uses alpha-beta pruning and depth-limited search to optimise the performance as much as possible, but due to the inherent limitations of the algorithm and Haskell, the performance is unfortunately less than ideal, especially if the board size gets bigger than 5-by-5. To mitigate the performance impact, the maximum level (which the user can pass in as an argument) can be reduced, but do not try using this AI with a board size that is too large.

## Explanation

`IC/` contains a Haskell unit test suite made by Imperial for testing functions, and `Tests.hs` contains the runner for the tests.

`Board.hs` contains definitions for the TicTacToe board with some utility functions related to the board
`Minimax.hs` contains implementation for the minimax agent, where `minimax` is the function that returns a new board with the agent's action added.
`TicTacToe.hs` contains the main function and all IO actions

## Credit

This project is based on a tutorial exercise for the first year Haskell course @ Imperial College London. The original tutorial exercise was implementing a basic two-player Tic Tac Toe game using Haskell's IO and monads. Significant modifications to the original exercise were made, but all credit for the skeleton of the project and the testing library goes to the Computing Department of Imperial College London.

The idea for this project came from following the [CS50's Introduction to Artificial Intelligence with Python](https://cs50.harvard.edu/ai/2020/) course by Harvard and edX. The course teaches the Minimax algorithm and uses this concept to develop a simple AI for a 3-by-3 Tic Tac Toe. I extended this basic program by using a functional programming language i.e. Haskell and generalising the board's size to N-by-N.
