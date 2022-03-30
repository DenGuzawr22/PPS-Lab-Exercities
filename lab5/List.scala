package u05lab.ex1

import u05lab.ex1.List

// Ex 1. implement the missing methods both with recursion or with using fold, map, flatMap, and filters
// List as a pure interface
enum List[A]:
  case ::(h: A, t: List[A])
  case Nil()
  def ::(h: A): List[A] = List.::(h, this)

  def head: Option[A] = this match
    case h :: t => Some(h)
    case _ => None

  def tail: Option[List[A]] = this match
    case h :: t => Some(t)
    case _ => None

  def append(list: List[A]): List[A] = this match
    case h :: t => h :: t.append(list)
    case _ => list

  def foreach(consumer: A => Unit): Unit = this match
    case h :: t => consumer(h); t.foreach(consumer)
    case _ =>

  def get(pos: Int): Option[A] = this match
    case h :: t if pos == 0 => Some(h)
    case h :: t if pos > 0 => t.get(pos - 1)
    case _ => None

  def filter(predicate: A => Boolean): List[A] = this match
    case h :: t if predicate(h) => h :: t.filter(predicate)
    case _ :: t => t.filter(predicate)
    case _ => Nil()

  def map[B](fun: A => B): List[B] = this match
    case h :: t => fun(h) :: t.map(fun)
    case _ => Nil()

  def flatMap[B](f: A => List[B]): List[B] =
    foldRight[List[B]](Nil())(f(_) append _)

  def foldLeft[B](z: B)(op: (B, A) => B): B = this match
    case h :: t => t.foldLeft(op(z, h))(op)
    case Nil() => z

  def foldRight[B](z: B)(f: (A, B) => B): B = this match
    case h :: t => f(h, t.foldRight(z)(f))
    case _ => z

  def length: Int = foldLeft(0)((l, _) => l + 1)

  def isEmpty: Boolean = this match
    case Nil() => true
    case _ => false

  def reverse(): List[A] = foldLeft[List[A]](Nil())((l, e) => e :: l)

  /** EXERCISES */
  def zipRight: List[(A, Int)] =
    def _zipRight(l: List[A], n: Int): List[(A, Int)] = l match
          case h :: t:List[A] => (h, n) :: _zipRight(t, n+1)
          case _ => Nil()
    _zipRight(this, 0)

  def zipRight2: List[(A, Int)] = this.reverse() match
    case h :: t => t.foldLeft((h, this.length - 1) :: Nil())(( l, head) => l match
      case (v,i) :: t => ((head, i - 1) :: Nil()).append(l))


  def partition(pred: A => Boolean): (List[A], List[A]) = (this.filter(pred), this.filter(A => !pred(A)))

  def span(pred: A => Boolean): (List[A], List[A]) =
    @annotation.tailrec
    def findFirstFalseIndex(list: List[(A,Int)]): Int = list match
      case (h,i) :: t if pred(h) => findFirstFalseIndex(t)
      case (h,i) :: t => i
      case _ => Int.MaxValue
    val spanIndex = findFirstFalseIndex(this.zipRight)
    this.zipRight.partition((h,i) => i < spanIndex) match
      case (l1, l2) => (l1.map((A,Int) => A), l2.map((A,Int) => A))

  def span2(pred: A => Boolean): (List[A], List[A]) =
    @annotation.tailrec
    def _span2(leftList: List[A])(remainList: List[A]): (List[A], List[A]) = remainList match
      case h :: t if pred(h) => _span2(leftList.append(h :: Nil()))(t)
      case h :: t => (leftList, remainList)
      case _ => (leftList, Nil())
    _span2(Nil())(this)


  /** @throws UnsupportedOperationException if the list is empty */
  def reduce(op: (A, A) => A): A =
    if this.isEmpty then throw new UnsupportedOperationException
    def _reduce(l: List[A]): A = l match
      case h :: t if t.isEmpty => h
      case h :: t => op(h, _reduce(t))
    _reduce(this)

  def reduce2(op: (A, A) => A): A = this match
      case h :: t => t.foldRight(h)(op)
      case _ => throw new UnsupportedOperationException

  def takeRight(n: Int): List[A] =
    @annotation.tailrec
    def _takeRight(n_skip: Int)(l: List[A]): List[A] = l match
      case h :: t if n_skip > 0 => _takeRight(n_skip-1)(t)
      case h :: t => t
      case _ => Nil()
    _takeRight(this.length -1 -n)(this)

  def takeRight2(n: Int): List[A] = this.zipRight.filter((h,i) => i > this.length-1-n ).map((h,i) => h)


// Factories
object List:

  def apply[A](elems: A*): List[A] =
    var list: List[A] = Nil()
    for e <- elems.reverse do list = e :: list
    list

  def of[A](elem: A, n: Int): List[A] =
    if n == 0 then Nil() else elem :: of(elem, n - 1)

@main def checkBehaviour(): Unit =
  val reference = List(1, 2, 3, 4)
  println(reference.zipRight) // List((1, 0), (2, 1), (3, 2), (4, 3))
  println(reference.partition(_ % 2 == 0)) // (List(2, 4), List(1, 3))
  println(reference.span(_ % 2 != 0)) // (List(1), List(2, 3, 4))
  println(reference.span(_ < 3)) // (List(1, 2), List(3, 4))
  println(reference.reduce(_ + _)) // 10
  try Nil.reduce[Int](_ + _)
  catch case ex: Exception => println(ex) // prints exception
  println(List(10).reduce(_ + _)) // 10
  println(reference.takeRight(3)) // List(2, 3, 4)
