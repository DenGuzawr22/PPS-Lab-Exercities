package u03

enum Stream[A]:
  private case Empty()
  private case Cons(sh: () => A, st: () => Stream[A])

object Stream {
  import u03.lab3.Lists.*

  def cons[A](h: => A, t: => Stream[A]): Stream[A] =
    lazy val _h = h
    lazy val _t = t
    Cons( () => _h, () => _t)

  def empty[A](): Stream[A] = Empty()

  def sum(stream: Stream[Int]): Int = stream match
    case Cons(sh, st) => sh() + sum(st())
    case Empty() => 0

  def take[A](stream: Stream[A])(size: Int): Stream[A] = stream match
    case Cons(sh, st) if size > 0 => Cons(sh, () => take(st())(size-1))
    case _ => Empty()

  def iterate[A](initial: A)(f: A => A): Stream[A] =
    cons(initial, iterate(f(initial))(f))

  def toList[A](stream: Stream[A]): List[A] = stream match
    case Cons(sh, st) => List.Cons(sh(), toList(st()))
    case Empty() => List.Nil()

  def drop[A](stream: Stream[A])(n: Int): Stream[A] = stream match
    case Cons(sh, st) if n > 0 => drop(st())(n-1)
    case Cons(sh, st) => stream
    case _ => Empty()

  def constant[A](v: A): Stream[A] = Stream.iterate(v)(a => a)

  def fibs(): Stream[Int] =
    def _fibs(a: Int, b: Int)(f: (Int, Int) => Int): Stream[Int] =
      cons(a, _fibs(b,f(a,b))(f))
    _fibs(0,1)(_ + _)
}
