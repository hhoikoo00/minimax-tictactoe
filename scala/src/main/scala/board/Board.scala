package board

import board.Cell._
import board.Player._

/**
 * Represents a n-by-n TicTacToe board with helper methods for playing the game
 *
 * @param dim     width and height of the board (N)
 * @param _player a private variable initializing the starting player
 */
class Board(val dim: Int, private var _player: Player) {
  require(dim > 2)

  type Position = Int

  private val size = dim * dim
  val board: Array[Cell] = Array.fill[Cell](size)(Empty)

  def player: Player = _player

  /*
   * Utility functions
   */

  /**
   * Returns an array of rows (which itself is an array) in the board
   *
   * @return a 2d array of Cell each representing a row
   */
  def rows: Array[Array[Cell]] = for (i <- (0 until dim).toArray) yield {
    for (j <- (0 until dim).toArray) yield {
      board(i * dim + j)
    }
  }

  /**
   * Returns an array of columns (which itself is an array) in the board
   *
   * @return a 2d array of Cell each representing a column
   */
  def cols: Array[Array[Cell]] = rows.transpose

  /**
   * Returns an array of diagonals (which itself is an array) in the board
   *
   * @return a 2d array of Cell each representing a diagonal
   */
  def diags: Array[Array[Cell]] = Seq(
    (for (i <- 0 until dim) yield board(i * dim + i)).toArray,
    (for (i <- 0 until dim) yield board(i * dim + ((dim - 1) - i))).toArray
  ).toArray

  /**
   * Returns a string representation of the board as a 2d grid
   *
   * @return string representation of the board
   */
  override def toString: String = rows
    .map(row => row.map(_.toString).reduce((a, b) => s"$a $b"))
    .reduce((r1, r2) => s"$r1\n$r2")

  /*
   * Minimax helper functions
   */

  /**
   * Mark the player's move at the given position; move must be valid
   *
   * @param move   position to apply the move
   * @param player player that makes the move
   */
  def applyMove(move: Position, player: Player): Unit = {
    require(validPosition(move) && board(move) == Empty)

    board(move) = Taken(player)
    _player = _player.opponent
  }

  /**
   * Undo a valid move at the given position by marking it empty
   *
   * @param move position to unapply the move
   */
  def unapplyMove(move: Position): Unit = {
    require(validPosition(move) && board(move) != Empty)

    board(move) = Empty
    _player = _player.opponent
  }

  /**
   * Returns a Seq of all possible actions represented as Position values
   *
   * @return Seq of all possible actions
   */
  def actions: Seq[Position] = (0 until size).filter(board(_) == Empty)

  /**
   * Returns whether a position is valid
   *
   * @param move Int value of a potential board position to check
   * @return Boolean value of whether the move is valid
   */
  def validPosition(move: Position): Boolean = 0 <= move && move < size

  /**
   * Returns whether the board is in its finished status
   *
   * @return Boolean value of whether the board is at the end
   */
  def terminal: Boolean = !board.contains(Empty) || math.abs(utility) == dim

  /**
   * Calculate the expected utility, which is the maximum number of uninterrupted squares
   * a player has, returning a value for whichever player has the larger value
   * X max player (value +ve) ; O min player (value -ve)
   *
   * @return The utility value of the board
   */
  def utility: Int = (rows ++ cols ++ diags)
    // Get count of all Xs and Os
    .map(_.foldLeft((0, 0))((t, c) => t match {
      case (x, o) => c match {
        case Taken(X) => (x + 1, o)
        case Taken(O) => (x, o + 1)
        case Empty => (x, o)
      }
    }))
    // Only get rows that only have Os or Xs
    .filter { case (x, o) => (x == 0 && o != 0) || (x != 0 && o == 0) }
    // Get the maximum o and x value and return the utility
    .foldLeft((0, 0))((s, t) => (math.max(s._1, t._1), math.max(s._2, t._2)))
  match {
    case (x, o) => if (x == o) if (player == X) x else -o else if (x > o) x else -o
  }
}
