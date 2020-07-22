package agent

import board._

abstract class AbstractPlayer(val board: Board, val player: Player) {
  def makeMove(): Unit
}
