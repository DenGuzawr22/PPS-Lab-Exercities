package lab

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions.{assertEquals}

//Recursion
class Es6 {

  def fib(n: Int): Int = n match
    case 0 => 0
    case 1 => 1
    case _ => fib(n - 1) + fib(n - 2)

  def fibTail(n: Int): Int =
    @annotation.tailrec
    def _fib(n: Int, accA: Int, accB: Int): Int = n match
      case 0 => accA
      case _ => _fib(n - 1, accB, accA + accB)
    _fib(n,0,1)

  @Test def areFibWorking(): Unit =
    val fibFunctions = List(fib, fibTail)
    fibFunctions.foreach(f => {
      assertEquals(0, f(0))
      assertEquals(1, f(1))
      assertEquals(1, f(2))
      assertEquals(2, f(3))
      assertEquals(3, f(4))
    })
}
