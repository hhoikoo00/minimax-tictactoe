package main

import board.Board

object TicTacToe {
  def main(args: Array[String]): Unit = {
    println("Welcome to Tic Tac Toe on an N x N board!")

    /* Initialize a new board and both players */
    val board = new Board(
      Parser.boardSize(),
      Parser.startPlayer())
    val players = Parser.players(board)

    /* Play the game */
    while (!board.terminal) {
      println(board)
      players(board.player).makeMove()
    }

    /* Display the result */
    println(board)
    board.winner match {
      case Some(p) => println(s"Player $p won!")
      case None => println("Draw: no player won.")
    }
    println("Thank you for playing the game")
  }
}
