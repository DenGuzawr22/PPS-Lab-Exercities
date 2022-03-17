package lab2

import org.junit.jupiter.api.Assertions.{assertEquals}
import org.junit.jupiter.api.{Test}

//Compositions
class Es5 {
  val fInt: Int => Int = x => x - 1
  val gInt: Int => Int = x => x * 2

  def compose(f: Int => Int, g: Int => Int): Int => Int = (x: Int) => f(g(x))
  def genericCompose[A](f: A => A, g: A => A): A => A = (x: A) => f(g(x))
  def superGenericCompose[A, B, C](f: B => C, g: A => B): A => C = (x: A) => f(g(x))

  @Test def canCompose(): Unit =
    assertEquals(9, compose(fInt, gInt)(5))

  @Test def canGenericComposeIntFun(): Unit =
    assertEquals(9, genericCompose(fInt, gInt)(5))

  @Test def canGenericComposeStringFun(): Unit =
    val fString: String => String = "a" concat _
    val gString: String => String = _ concat "cc"
    assertEquals("abcc", genericCompose(fString, gString)("b"))

  @Test def testSuperGenericCompose(): Unit =
    val f: Int => Int = _ + 100
    val g: String => Int = _.length
    assertEquals(104, superGenericCompose(f, g)("bbbb"))
}
