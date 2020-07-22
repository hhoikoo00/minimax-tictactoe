package board

sealed trait Player {
  def opponent: Player
}

/**
 * Enum representing a TicTacToe player
 */
object Player {

  case object O extends Player {
    override def toString: String = "O"

    override def opponent: Player = X
  }

  case object X extends Player {
    override def toString: String = "X"

    override def opponent: Player = O
  }

}
