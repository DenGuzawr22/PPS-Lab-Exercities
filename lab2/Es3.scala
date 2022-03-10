package lab

import org.junit.jupiter.api.Assertions.{assertEquals, assertFalse, assertNotEquals, assertTrue}
import org.junit.jupiter.api.{Test}

class Es3 {
  // 3.a)
  def parity(x: Int): String = x match
    case n if n%2 == 0  => "even"
    case _ => "odd"

  val parityVal = (x: Int) => x match
    case n if n%2 == 0  => "even"
    case _ => "odd"

  //3.b)
  def neg(pred: String => Boolean ): String => Boolean = !pred(_)
  val negGenericVal: (String => Boolean) => (String => Boolean) = pred => !pred(_)

  //3.c)
  def negGeneric[A](pred: A => Boolean): A => Boolean = !pred(_)

  @Test def parityCheckOdd(): Unit =
    val odd: String = "odd"
    val oddNumber: Int = 11
    assertEquals(odd, parity(oddNumber))
    assertEquals(odd, parityVal(oddNumber))

  @Test def parityCheckEven(): Unit =
    val even: String = "even"
    val evenNumber: Int = 10
    assertEquals(even, parity(evenNumber))
    assertEquals(even, parityVal(evenNumber))

  @Test def isWorkingNeg(): Unit =
    val empty: String => Boolean = _ == ""
    val notEmpty = neg(empty)
    val text: String = "text"
    val emptyString: String = ""
    assertNotEquals(notEmpty(text), empty(text))
    assertNotEquals(notEmpty(emptyString), empty(emptyString))
    assertNotEquals(notEmpty(text), notEmpty(emptyString))

  @Test def genericNegCheckInt(): Unit =
    val intToTrue: Int => Boolean = _ => true
    val intVal: Int = 999
    assertTrue(intToTrue(intVal))
    assertFalse(negGeneric(intToTrue)(intVal))

  @Test def genericNegCheckString(): Unit =
    val stringToTrue: String => Boolean = _ => true
    val stringVal: String = ""
    assertTrue(stringToTrue(stringVal))
    assertFalse(negGeneric(stringToTrue)(stringVal))

}
