package lab2

import org.junit.jupiter.api.Assertions.{assertFalse,assertTrue}
import org.junit.jupiter.api.{Test}

//Currying
class Es4 {
  val p1: Int => Int => Int => Boolean = x => y => z => x <= y && y <= z
  val p2: (Int, Int, Int) => Boolean = (x, y, z) => x <= y && y <= z
  def p3(x: Int)(y: Int)(z: Int): Boolean = x <= y && y <= z
  def p4(x: Int, y: Int, z: Int): Boolean = x <= y && y <= z

  @Test def checkCurriedFunType(): Unit =
    assertTrue(p1(1)(2)(3))
    assertFalse(p1(3)(2)(1))

  @Test def checkNotCurriedFunType(): Unit =
    assertTrue(p2(1,2,3))
    assertFalse(p2(3,2,1))

  @Test def checkCurriedFunction(): Unit =
    assertTrue(p3(1)(2)(3))
    assertFalse(p3(3)(2)(1))

  @Test def checkNotCurriedFunction(): Unit =
    assertTrue(p4(1,2,3))
    assertFalse(p4(3,2,1))
}
