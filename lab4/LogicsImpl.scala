package u04lab.polyglot.a01b

import scala.jdk.javaapi.OptionConverters
import u04lab.polyglot.OptionToOptional
import u04lab.code.Option
import u04lab.code.Option.*
import u04lab.code.List.*
import u04lab.code.List
import java.util.Random
import java.lang.Math


class LogicsImpl(private val size: Int, private val mines: Int) extends Logics:
  private var bombs: List[(Int, Int)] = Nil()
  private var hits: Int = 0
  def won: Boolean = hits >= size * size - mines


  def hit(x: Int, y: Int): java.util.Optional[Integer] = bombs match
    case Cons(h, t) => OptionToOptional(checkCell(x, y))
    case _ => setupAndHit(x, y);

  def setupAndHit(x: Int, y: Int): java.util.Optional[Integer] =
    @annotation.tailrec
    def addBomb(): Unit =
      @annotation.tailrec
      def createBomb(): List[(Int, Int)] =
        val ran = Random()
        val bomb = (ran.nextInt(size), ran.nextInt(size))
        if contains(bombs, bomb) || bomb == (x,y) then  createBomb() else Cons(bomb,Nil())
      bombs match
        case Nil() => bombs = createBomb(); addBomb()
        case Cons(h, t) if length(bombs) < mines => bombs = append(bombs, createBomb()); addBomb()
        case _ =>
    addBomb()
    println(bombs)
    hit(x,y)

  def checkCell(x: Int, y: Int): Option[Int] =
    def countBombs(x: Int, y: Int, l:  List[(Int, Int)] ): Int = l match
      case Cons(h: (Int, Int), t) => countBombs(x,y, t) + (h match
        case (bomb_x, bomb_y) if (x == bomb_x && Math.abs(y - bomb_y) < 2)
                              || (y == bomb_y && Math.abs(x - bomb_x) < 2)
                              || (Math.abs(y - bomb_y) ==1 && Math.abs(x - bomb_x) == 1) => 1
        case _ => 0)
      case _ => 0
    if contains(bombs, (x, y)) then
      None()
    else
      hits += 1
      Some(countBombs(x, y, bombs))




