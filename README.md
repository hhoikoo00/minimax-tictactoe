# haskell_tictactoe_minimax

## Introduction

This is a personal peroject that implements a Minimax agent for a generic N-by-N sized Tic Tac Toe game.

## Disclaimer

The minimax agent uses alpha-beta pruning and depth-limited search to optimise the performance as much as possible, but due to the inherent limitations of the algorithm and Haskell, the performance is unfortunately less than ideal, especially if the board size gets bigger than 5-by-5. To mitigate the performance impact, the maximum level (which is set as a constant in the source code) can be reduced, but do not try using this AI with a board size too large.

## Credit

This project is based on a tutorial exercise for the first year Haskell course @ Imperial College London. The original tutorial exercise was about implementing a basic two-player Tic Tac Toe game, and the aim of this exercise was to familiarise the students with Haskell's IO and monads. All credit for the tutorial exercise goes to the Computing department, thus the skeleton of this project and the test suite is copyrighted by the Computing Department of Imperial College London.

The idea for this project came from me following the [CS50's Introduction to Artificial Intelligence with Python](https://cs50.harvard.edu/ai/2020/) course by Harvard and edX. The course teaches the Minimax algorithm and uses this concept to develop a simple AI for a 3-by-3 Tic Tac Toe. I extended this basic program by using a functional programming language i.e. Haskell and generalising the board's size to N-by-N.
