package agent

import board._

import scala.util.Random

class ComputerPlayer(board: Board, player: Player) extends AbstractPlayer(board, player) {
  override def makeMove(): Unit = {
    //TODO Implement minimax agent for the computer
    
    println(s"""${board.player}'s turn: computer "thinking"...""")
    board.applyMove(board.actions(new Random().nextInt(board.actions.length)), player)
  }
}
