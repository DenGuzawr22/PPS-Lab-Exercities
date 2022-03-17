package u03.lab3

import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.Test
import u03.lab3.Lists.*
import List.*
import u03.Stream


class StreamTest {

  @Test def testDrop() =
    val s = Stream.take(Stream.iterate(0)(_ + 1))(10)
    val dropetList = Cons(6, Cons(7, Cons(8, Cons(9, Nil()))))
    assertEquals(dropetList, Stream.toList(Stream.drop(s)(6)))

  @Test def testConstant() =
    val xList = Cons("x", Cons("x", Cons("x", Cons("x", Cons("x", Nil())))))
    assertEquals(xList, Stream.toList(Stream.take(Stream.constant("x"))(5)))

  @Test def testFibonacciStream() =
    val fibonacciList = Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Cons(8, Cons(13, Nil()))))))))
    assertEquals(fibonacciList, Stream.toList(Stream.take(Stream.fibs())(8)))

}
