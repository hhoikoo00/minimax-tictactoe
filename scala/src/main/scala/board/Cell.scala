package board

sealed trait Cell

/**
 * An Enum representing a single cell in a TicTacToe board
 * Either Empty or Taken by a given player
 */
object Cell {

  case object Empty extends Cell {
    override def toString: String = "-"
  }

  case class Taken(p: Player) extends Cell {
    override def toString: String = p.toString
  }

}


