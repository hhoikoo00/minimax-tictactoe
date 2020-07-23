package agent

import board.Player._
import board._

import scala.util.Random

class ComputerPlayer(
  board: Board, player: Player, val levelMax: Int
) extends AbstractPlayer(board, player) {

  override def makeMove(): Unit = {
    println(s"""${board.player}'s turn: computer "thinking"...""")
    board.applyMove(minimax())
  }

  /*
   * Random AI
   */

  private def random(): Int = board.actions(new Random().nextInt(board.actions.length))

  /*
   * Minimax AI: player X is positive, player O is negative
   */

  private val MAX_UTILITY = board.dim + 1
  private val MIN_UTILITY = -(board.dim + 1)

  private def minimax(): Int = if (player == X)
    maxValue(0, MAX_UTILITY)
  else
    minValue(0, MIN_UTILITY)

  private def maxValue(level: Int, prevUtilityMin: Int): Int = {
    if (level >= levelMax || board.terminal)
      board.utility
    else {
      // Inductive case
      var utilityMax = MIN_UTILITY
      var actionMax = -1
      var alphaBeta = false

      for (action <- board.actions) {
        if (!alphaBeta) {
          // Apply move and recurse to find its util
          board.applyMove(action)
          val utility = minValue(level + 1, utilityMax)
          board.unapplyMove(action)

          // if new maximum found update
          if (utility > utilityMax) {
            utilityMax = utility
            actionMax = action
          }

          // Alpha-beta pruning
          if (utilityMax > prevUtilityMin) {
            alphaBeta = true
          }
        }
      }

      // If at the top level return the action; else return the utility
      if (level == 0) actionMax else utilityMax
    }
  }

  private def minValue(level: Int, prevUtilityMax: Int): Int = {
    if (level >= levelMax || board.terminal)
      board.utility
    else {
      // Inductive case
      var utilityMin = MAX_UTILITY
      var actionMin = -1
      var alphaBeta = false

      for (action <- board.actions) {
        if (!alphaBeta) {
          // Apply move and recurse to find its util
          board.applyMove(action)
          val utility = maxValue(level + 1, utilityMin)
          board.unapplyMove(action)

          // if new minimum found update
          if (utility < utilityMin) {
            utilityMin = utility
            actionMin = action
          }

          // Alpha-beta pruning
          if (utilityMin < prevUtilityMax) {
            alphaBeta = true
          }
        }
      }

      // If at the top level return the action; else return the utility
      if (level == 0) actionMin else utilityMin
    }
  }
}
