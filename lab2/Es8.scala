package lab

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions.*

class Es8 {
  enum Option[A]:
    case Some(a: A)
    case None()

  import Option.*

  def filter[A](value: Option[A])(f: A => Boolean): Option[A] = value match
    case Some(v: A) if f(v) => value
    case _ => None()

  def map[A](value: Option[A])(f: A => Boolean): Option[Boolean] = value match
    case Some(v: A) if f(v) => Some(true)
    case Some(v: A) if !f(v) => Some(false)
    case _ => None()

  def map2[A](op1: Option[A], op2: Option[A])(f: (A, A) => A): Option[A] =
    def _isPresent(op: Option[A]): Boolean = op match
      case Some(x) => true
      case _ => false
    def _getValue(op: Option[A]): A  = op match
      case Some(x) => x
    _isPresent(op1) && _isPresent(op2) match
      case true => Some(f(_getValue(op1),_getValue(op2)))
      case _ => None()

  val value: Some[Int]  = Some(5)
  val none: None[Int] = None[Int]()
  val greaterThanZero: Int => Boolean = _ > 0
  val lessThanZero: Int => Boolean = _ < 0

  @Test def canFilterKeepValue(): Unit =
    assertEquals(value, filter(value)(greaterThanZero))

  @Test def canFilterSkipValue(): Unit =
    assertEquals(None(), filter(value)(lessThanZero))
    assertEquals(None(), filter(none)(greaterThanZero))

  @Test def canMapTrueValue(): Unit =
    assertEquals(Some(true), map(value)(greaterThanZero))

  @Test def canMapFalseValue(): Unit =
    assertEquals(Some(false), map(value)(lessThanZero))

  @Test def canMapNoneValue(): Unit =
    assertEquals(None(), map(none)(greaterThanZero))

  @Test def canMap2NoneValue(): Unit =
    assertEquals(None(), map2(value, none)(_ + _))

  @Test def canMap2AddValues(): Unit =
    assertEquals(Some(10), map2(value, value)(_ + _))
}
