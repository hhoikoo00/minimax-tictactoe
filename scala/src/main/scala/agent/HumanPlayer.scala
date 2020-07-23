package agent

import board._
import main.Parser

import scala.io.StdIn
import scala.util.Try

class HumanPlayer(board: Board, player: Player) extends AbstractPlayer(board, player) {
  override def makeMove(): Unit = {
    print(s"Player $player make your move (row col) > ")
    val move = Parser.retryUntilSuccess {
      (for {
        input <- Try(StdIn.readLine().trim.split(" "))
        x = input(0).toInt
        y = input(1).toInt
      } yield x * board.dim + y).filter(board.validPosition)
    } { _ => print("Invalid move, try again: ") }
    board.applyMove(move)
  }
}
