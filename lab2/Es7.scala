package lab

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions.*

//Modules
class Es7 {
  enum Shape:
    case Rectangle(width: Double, height: Double)
    case Square(width: Double, height: Double)
    case Circle(radius: Double)

  object Shape:
    def perimeter(shape: Shape): Double = shape match
      case Rectangle(w, h) => w + h
      case Square(w, h) => w + h
      case Circle(r) => 2 * r *  math.Pi

    def area(shape: Shape): Double = shape match
      case Rectangle(w, h)  => w * h
      case Square(w, h)  => w * h
      case Circle(r) => math.Pi * math.pow(r,2)

  import Shape.*

  val rectangle: Rectangle = Rectangle(10,2)
  val square: Square = Square(10,2)
  val circle: Circle = Circle(10)

  @Test def checkRectanglePerimeter(): Unit =
    assertEquals(12, Shape.perimeter(rectangle))

  @Test def checkRectangleArea(): Unit =
    assertEquals(20, Shape.area(rectangle))

  @Test def checkSquarePerimeter(): Unit =
    assertEquals(12, Shape.perimeter(square))

  @Test def checkSquareArea(): Unit =
    assertEquals(20, Shape.area(square))

  @Test def checkCirclePerimeter(): Unit =
    assertEquals(62.8318, Shape.perimeter(circle), 0.0001)

  @Test def checkCircleArea(): Unit =
    assertEquals(314.1592, Shape.area(circle), 0.0001)
}
