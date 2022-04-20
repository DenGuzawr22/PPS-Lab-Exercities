package u06lab.code

import java.lang.StackWalker
import java.util.OptionalInt
import scala.annotation.tailrec

object ConnectThree extends App:
  val bound = 3
  val BOARD_SIZE = 4
  enum Player:
    case X, O
    def other: Player = this match
      case X => O
      case _ => X

  case class Disk(x: Int, y: Int, player: Player)
  /**
   * Board:
   * y
   *
   * 3
   * 2
   * 1
   * 0
   *   0 1 2 3 <-- x
   */
  type Board = Seq[Disk]
  type Game = Seq[Board]

  import Player.*

  @tailrec
  def find(board: Board, x: Int, y: Int): Option[Player] = board match
    case b if b.isEmpty => None
    case b if b.head.x == x && b.head.y == y => Some(b.head.player)
    case b if b.tail.nonEmpty => find(b.tail, x, y)
    case _ => None

  //Assume that the board is always correct and there is not a need to do additional controls
  def firstAvailableRow(board: Board, x: Int): Option[Int] = board.filter(d => d.x == x) match
    case diskCol if diskCol.length == BOARD_SIZE => None
    case diskCol => Option(diskCol.length)


  def placeAnyDisk(board: Board, player: Player): Seq[Board] =
    if board.length >= BOARD_SIZE*BOARD_SIZE then throw IllegalStateException("Board is already full")
    var newBoards: Game = Seq()
    for i <- 0 to bound do
      val elem = firstAvailableRow(board,i)
      if elem.nonEmpty then newBoards = newBoards :+ (board :+ Disk(i, elem.get, player))
    newBoards

  def computeAnyGame(player: Player, moves: Int): LazyList[Game] = moves match
    case 0 =>  LazyList(Seq(Seq()))
    case _ =>
      val games  = computeAnyGame(player.other, moves - 1)
      (for
        game <- games
        if !isWon(game.head)
        board <- placeAnyDisk(game.head, player)
      yield
        board +: game) ++ games.filter(g => isWon(g.head))

  def isWon(board: Board): Boolean = board match
    case b if b.isEmpty => false
    case b =>
      val d = board.last //get the previous move (disk)
      var won = false
      for
        i <- 0 to 1
        j <- -1 to 1
        if i == 0 && j == -1 || i == 1
        if !won
        //check of direction with coordinate increment +i, +j and -i, -j
        if isWonInDirection(d.player, board, d.x, d.y)(i, j) 
      do
        won = true
      won

  def isWonInDirection(p: Player, board: Board, x: Int, y: Int)(xInc: Int, yInc: Int): Boolean =
    @tailrec
    def countPath(n: Int, xx: Int, yy: Int, opX: Int => Int, opY: Int => Int): Int = find(board, opX(xx), opY(yy)) match
      case Some(player) if player == p && n <= 1 => 0
      case Some(player) if player == p => countPath(n-1, opX(xx), opY(yy), opX, opY)
      case _ => n
    countPath(countPath(bound - 1, x, y, _ - xInc, _ - yInc), x, y, _ + xInc, _ + yInc) == 0


  def printBoards(game: Seq[Board]): Unit =
    for
      y <- bound to 0 by -1
      board <- game.reverse
      x <- 0 to bound
    do
      print(find(board, x, y).map(_.toString).getOrElse("."))
      if x == bound then
        print(" ")
        if board == game.head then println()


// Exercise 4 (VERY ADVANCED!) -- modify the above one so as to stop each game when someone won!!
