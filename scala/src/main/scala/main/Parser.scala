package main

import agent._
import board.Player.{O, X}
import board._

import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object Parser {
  /**
   * Tries an action (op) until Success and returns its value; on fail also calls onWrong
   *
   * @param op An action to repeat until it is Successful
   * @param onWrong A function to call if Failure (may take in the error)
   * @tparam T Return type
   * @return The return value of op function if Success
   */
  @scala.annotation.tailrec
  def retryUntilSuccess[T](op: => Try[T])(onWrong: Throwable => Any = _ => ()): T =
    op match {
      case Success(value) => value
      case Failure(error) => onWrong(error); retryUntilSuccess(op)(onWrong)
    }

  /**
   * Parses user input until a valid board size inputted (n > 2)
   *
   * @return A valid board size from the user
   */
  def boardSize(): Int = {
    print("Enter the board size (N > 2) > ")
    retryUntilSuccess {
      Try(StdIn.readInt()).filter(_ > 2)
    } { _ => print("Invalid N size, try again > ") }
  }

  /**
   * Parses user input until a valid starting player inputted (X or O)
   *
   * @return A valid Player that is the starting player
   */
  def startPlayer(): Player = {
    print("""Which player should go first? "X" or "O"?: """)
    retryUntilSuccess {
      Try(StdIn.readLine().toUpperCase() match {
        case "O" => Player.O
        case "X" => Player.X
      })
    } { _ => print("""Please type either "X" or "O": """) }
  }

  /**
   * Returns a Map of players (O and X), each initialized according to user input and
   * whether they are a Human Player or a Computer Player
   *
   * @param board Board to initialize the Players with
   * @return The Map of Players
   */
  def players(board: Board): Map[Player, AbstractPlayer] =
    Map[Player, AbstractPlayer](X -> player(board, X), O -> player(board, O))

  private def player(board: Board, player: Player): AbstractPlayer = {
    print(s"""is $player a human player ("H") or a computer player ("C")? > """)
    retryUntilSuccess {
      Try(StdIn.readLine().toUpperCase() match {
        case "C" => new ComputerPlayer(board, player)
        case "H" => new HumanPlayer(board, player)
      })
    } { _ => print("""Please type either "C" or "H": """) }
  }
}
