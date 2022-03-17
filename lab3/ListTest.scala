package u03.lab3

import org.junit.*
import org.junit.Assert.*
import u03.lab3.Lists.*
import u02.Optionals.Option.*
import u02.Modules.Person.*

class ListTest:
  import List.*

  val l: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum() =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(l))

  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_ + ""))

  @Test def testFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_ != 20))

  val lst = Cons(10, Cons(20, Cons(30, Nil())))
  @Test def testDrop() =
    assertEquals(drop(lst, 1), Cons(20, Cons(30, Nil())))
    assertEquals(drop(lst, 2), Cons(30, Nil()))
    assertEquals(drop(lst, 5), Nil())

  @Test def testAppend() =
    val tail = Cons (40, Nil())
    val appendedList = Cons(10, Cons(20, Cons(30, Cons(40, Nil()))))
    assertEquals(append(lst, tail), appendedList)

  @Test def testFlatMap() =
    val flatMapResult1 = Cons(11, Cons(21, Cons(31, Nil())))
    assertEquals(List.flatMap(lst)(v => Cons(v + 1, Nil())), flatMapResult1)
    val flatMapResult2 = Cons (11 , Cons (12 , Cons (21 , Cons (22 , Cons (31 , Cons (32 , Nil ()))))))
    assertEquals(List.flatMap(lst)(v => Cons(v + 1, Cons(v + 2, Nil ()))), flatMapResult2)

  @Test def testMax() =
    assertEquals(Some(25), max(Cons(10, Cons(25, Cons(20, Nil())))))
    assertEquals(max(Nil()), None())

  @Test def testExtractCourses() =
    val t1 = Teacher("A", "OOP")
    val t2 = Teacher("C", "IoT")
    val s1 = Student("B", 1900)
    val s2 = Student("X", 1901)
    val persons = Cons(t1, Cons(s1, Cons(s2, Cons(t2, Nil()))))
    val courses = Cons("OOP", Cons("IoT", Nil()))
    assertEquals(courses, extractCourses(persons))
  
  val lst2 = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
  
  @Test def testFoldLeft() =
    assertEquals(-16, foldLeft(lst2)(0)(_ - _))

  @Test def testFoldRight() =
    assertEquals(-8, foldRight(lst2)(0)(_ - _))
  