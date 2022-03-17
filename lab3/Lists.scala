package u03.lab3
import u02.Modules.Person
import u02.Optionals.Option.*
import u02.Optionals.*
import u02.Modules.Person.*

object Lists extends App :

  // A generic linkedlist
  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()

  // a companion object (i.e., module) for List
  object List:

    def sum(l: List[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    def drop[A](l: List[A], n: Int): List[A] = l match
      case Cons(h, t) if n > 0 => drop(t, n-1)
      case _ => l

    def append[A](left: List[A], right: List[A]): List[A] = left match
      case Cons(h, t) => Cons(h, append(t, right))
      case _ => right

    def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l match
      case Cons(h, t) => append(f(h), flatMap(t)(f));
      case _ => Nil()

    def map[A, B](l: List[A])(mapper: A => B): List[B] =
      flatMap(l)(A => Cons(mapper(A), Nil()))

    def filter[A](l1: List[A])(pred: A => Boolean): List[A] =
      flatMap(l1)(a => pred(a) match
        case true => Cons(a, Nil())
        case _ => Nil())

    def max(l: List[Int]): Option[Int] = l match
      case Cons(h, t) if h >= orElse(max(t),h) => Some(h)
      case Cons(h, t)  => max(t)
      case _ => None()

    def extractCourses(l: List[Person]): List[String] =
      flatMap(l)(_ match
        case Teacher(n, m) => Cons(m, Nil())
        case _ => Nil())

    def foldLeft[A, B](l: List[A])(acc: B)(f: (B,A) => B): B = l match
      case Cons(h, t) => foldLeft(t)(f(acc, h))(f)
      case _ => acc

    def foldRight[A](l: List[A])(acc: A)(f: (A,A) => A): A =
      def revers[A](l: List[A]): List[A] = l match
        case Cons(h, t) => append(revers(t), Cons(h, Nil()))
        case _ => Nil()
      foldLeft(revers(l))(acc)((a,b) => f(b,a))


