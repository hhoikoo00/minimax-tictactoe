package agent

import board.Player.X
import board._

import scala.util.Random

class ComputerPlayer(
  board: Board, player: Player, val level: Int
) extends AbstractPlayer(board, player) {

  private val MAX_UTILITY = board.dim + 1
  private val MIN_UTILITY = -(board.dim + 1)

  override def makeMove(): Unit = {
    //TODO Implement minimax agent for the computer

    println(s"""${board.player}'s turn: computer "thinking"...""")
    board.applyMove(board.actions(new Random().nextInt(board.actions.length)), player)
  }
}
