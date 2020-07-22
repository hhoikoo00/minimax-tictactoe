import board._
import board.Cell._
import board.Player._

object TestGround extends App {
  def printArray[T](array: Array[T]): Unit =
    println(array.map(_.toString).reduce((a, b) => s"$a $b"))

  val board = new Board(4, X)

  val r = scala.util.Random
  for (i <- board.board.indices) {
    board.board(i) = if (r.nextBoolean) Taken(X) else Taken(O)
  }

  println(board)

  println()

  board.rows.foreach(printArray)

  println()

  board.cols.foreach(printArray)

  println()

  board.diags.foreach(printArray)
}
