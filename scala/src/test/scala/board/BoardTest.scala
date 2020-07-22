package board

import board.Cell.{Empty, Taken}
import board.Player.{O, X}
import org.scalatest.funsuite.AnyFunSuite

class BoardTest extends AnyFunSuite {
  def initBoard(board: Board, elems: Seq[Cell]): Unit = {
    require(board.dim * board.dim == elems.length)

    var move = 0
    elems foreach {
      case Taken(p) => board.applyMove(move, p); move += 1
      case Empty => move += 1
    }
  }

  val testBoard1 = new Board(4, X)
  initBoard(testBoard1, Seq(
    Taken(O), Taken(X), Empty, Taken(O),
    Taken(O), Empty, Taken(X), Taken(X),
    Taken(O), Empty, Empty, Taken(X),
    Taken(O), Taken(X), Empty, Empty
  ))

  val testBoard1_ = new Board(4, X)
  initBoard(testBoard1_, Seq(
    Taken(O), Taken(X), Empty, Taken(O),
    Taken(O), Empty, Taken(X), Taken(X),
    Empty, Empty, Empty, Taken(X),
    Taken(O), Taken(X), Empty, Empty
  ))

  val testBoard2 = new Board(3, X)
  initBoard(testBoard2, Seq(
    Taken(X), Empty, Empty,
    Empty, Empty, Empty,
    Empty, Empty, Empty
  ))

  val testBoard3 = new Board(5, X)
  initBoard(testBoard3, Seq(
    Taken(O), Taken(X), Empty, Taken(O), Taken(X),
    Taken(O), Empty, Taken(X), Taken(X), Empty,
    Empty, Empty, Taken(X), Taken(O), Taken(O),
    Taken(O), Taken(X), Empty, Empty, Taken(X),
    Taken(X), Empty, Taken(O), Empty, Empty
  ))

  val testBoard3_ = new Board(5, X)
  initBoard(testBoard3_, Seq(
    Taken(O), Taken(X), Empty, Taken(O), Taken(X),
    Taken(O), Empty, Taken(X), Taken(X), Empty,
    Empty, Empty, Empty, Taken(O), Taken(O),
    Taken(O), Taken(X), Empty, Empty, Taken(X),
    Taken(X), Empty, Taken(O), Empty, Empty
  ))

  val testBoard4 = new Board(3, X)
  initBoard(testBoard4, Seq(
    Taken(O), Taken(X), Taken(O),
    Taken(X), Taken(O), Taken(O),
    Taken(X), Taken(O), Taken(X)
  ))

  val testBoard5 = new Board(3, X)

  val testBoard6 = new Board(4, X)
  initBoard(testBoard6, Seq(
    Empty, Empty, Taken(X), Taken(X),
    Empty, Taken(X), Empty, Empty,
    Empty, Empty, Taken(X), Taken(O),
    Taken(O), Taken(X), Taken(O), Taken(O)
  ))

  val testBoard6_ = new Board(4, X)
  initBoard(testBoard6_, Seq(
    Empty, Empty, Taken(X), Taken(X),
    Empty, Taken(X), Empty, Empty,
    Taken(O), Empty, Taken(X), Taken(O),
    Taken(O), Taken(X), Taken(O), Taken(O)
  ))

  ignore("rows") {

  }

  ignore("cols") {

  }

  test("diags") {
    assertResult(Array(
      Array(Taken(O), Empty, Empty, Empty),
      Array(Taken(O), Taken(X), Empty, Taken(O))
    ))(testBoard1.diags)
    assertResult(Array(
      Array(Taken(O), Empty, Taken(X), Empty, Empty),
      Array(Taken(X), Taken(X), Taken(X), Taken(X), Taken(X))
    ))(testBoard3.diags)
  }

  test("actions") {
    assertResult(Seq(2, 5, 9, 10, 14, 15))(testBoard1.actions)
    assertResult(Seq(2, 5, 8, 9, 10, 14, 15))(testBoard1_.actions)
    assertResult(1 to 8)(testBoard2.actions)
    assertResult(Seq(2, 6, 9, 10, 11, 17, 18, 21, 23, 24))(testBoard3.actions)
    assertResult(Seq(2, 6, 9, 10, 11, 12, 17, 18, 21, 23, 24))(testBoard3_.actions)
    assertResult(Seq())(testBoard4.actions)
    assertResult(0 to 8)(testBoard5.actions)
    assertResult(Seq(0, 1, 4, 6, 7, 8, 9))(testBoard6.actions)
    assertResult(Seq(0, 1, 4, 6, 7, 9))(testBoard6_.actions)
  }

  test("terminal") {
    assertResult(true)(testBoard1.terminal)
    assertResult(false)(testBoard1_.terminal)
    assertResult(false)(testBoard2.terminal)
    assertResult(true)(testBoard3.terminal)
    assertResult(false)(testBoard3_.terminal)
    assertResult(true)(testBoard4.terminal)
    assertResult(false)(testBoard5.terminal)
    assertResult(false)(testBoard6.terminal)
    assertResult(false)(testBoard6_.terminal)
  }

  test("utility") {
    assertResult(-4)(testBoard1.utility)
    assertResult(-3)(testBoard1_.utility)
    assertResult(1)(testBoard2.utility)
    assertResult(5)(testBoard3.utility)
    assertResult(4)(testBoard3_.utility)
    assertResult(0)(testBoard4.utility)
    assertResult(0)(testBoard5.utility)
    assertResult(2)(testBoard6.utility)
    assertResult(2)(testBoard6_.utility)
  }

  ignore("applyMove") {

  }

  ignore("unapplyMove") {

  }
}
