package agent

import board.Player._
import board._

import scala.util.Random

class ComputerPlayer(
  board: Board, player: Player, val levelMax: Int
) extends AbstractPlayer(board, player) {

  override def makeMove(): Unit = {
    println(s"""${board.player}'s turn: computer "thinking"...""")
    board.applyMove(minimax(), player)
  }

  /*
   * Random AI
   */

  private def random(): Int = board.actions(new Random().nextInt(board.actions.length))

  /*
   * Minimax AI
   */

  private val MAX_UTILITY = board.dim + 1
  private val MIN_UTILITY = -(board.dim + 1)

  private def minimax(): Int = if (player == X)
    maxValue(1, MAX_UTILITY)._1
  else
    minValue(1, MIN_UTILITY)._1

  private def maxValue(level: Int, prevUtilityMin: Int): (Int, Int) = {
    // Base case: board terminated or max level reached
    if (level > levelMax || board.terminal)
      (-1, board.utility)
    else {
      // Inductive case
      var utilityMax = MIN_UTILITY
      var actionMax = -1
      var alphaBeta = false

      for (action <- board.actions) {
        if (!alphaBeta) {
          // Apply move and recurse to find its util
          board.applyMove(action, board.player)
          val utility = minValue(level + 1, utilityMax)._2

          // if new maximum found update
          if (utility > utilityMax) {
            utilityMax = utility
            actionMax = action
          }

          // Reverse the move done for calculation
          board.unapplyMove(action)

          // Alpha-beta pruning
          if (utilityMax > prevUtilityMin) {
            alphaBeta = true
          }
        }
      }

      (actionMax, utilityMax)
    }
  }

  private def minValue(level: Int, prevUtilityMax: Int): (Int, Int) = {
    // Base case: board terminated or max level reached
    if (level > levelMax || board.terminal)
      (-1, board.utility)
    else {
      // Inductive case
      var utilityMin = MAX_UTILITY
      var actionMin = -1
      var alphaBeta = false

      for (action <- board.actions) {
        if (!alphaBeta) {
          // Apply move and recurse to find its util
          board.applyMove(action, board.player)
          val utility = maxValue(level + 1, utilityMin)._2

          // if new minimum found update
          if (utility < utilityMin) {
            utilityMin = utility
            actionMin = action
          }

          // Reverse the move done for calculation
          board.unapplyMove(action)

          // Alpha-beta pruning
          if (utilityMin < prevUtilityMax) {
            alphaBeta = true
          }
        }
      }

      (actionMin, utilityMin)
    }
  }
}
