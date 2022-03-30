package u05lab.ex1

import org.junit.Test
import org.junit.Assert.*

class ListTest:
  val reference: List[Int] = List(1, 2, 3, 4)

  @Test def testZipRight(): Unit =
    assertEquals(List((1, 0), (2, 1), (3, 2), (4, 3)), reference.zipRight)

  @Test def testPartition(): Unit =
    assertEquals((List(2, 4), List(1, 3)), reference.partition(_ % 2 == 0))

  @Test def testSpan(): Unit =
    assertEquals((List(1), List(2, 3, 4)), reference.span(_ % 2 != 0))
    assertEquals((List(1, 2), List(3, 4)), reference.span(_ < 3))

  @Test def testSpan2(): Unit =
    assertEquals((List(1), List(2, 3, 4)), reference.span2(_ % 2 != 0))
    assertEquals((List(1, 2), List(3, 4)), reference.span2(_ < 3))

  val emptyList: List[Int] = List.Nil()

  @Test def testReduce(): Unit =
    assertEquals(10, reference.reduce(_ + _ ))
    assertEquals(10, List(10).reduce(_ + _))
    assertThrows( classOf[UnsupportedOperationException], () => emptyList.reduce(_ + _))

  @Test def testReduce2(): Unit =
    assertEquals(10, reference.reduce2(_ + _ ))
    assertEquals(10, List(10).reduce(_ + _))
    assertThrows( classOf[UnsupportedOperationException], () => emptyList.reduce2(_ + _))

  @Test def testTakeRight(): Unit =
    assertEquals(List(2, 3, 4), reference.takeRight(3))

  @Test def testTakeRight2(): Unit =
    assertEquals(List(2, 3, 4), reference.takeRight2(3))

