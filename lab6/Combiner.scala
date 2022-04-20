package u06lab.code

/** 1) Implement trait Functions with an object FunctionsImpl such that the code in TryFunctions works correctly. */

trait Functions:
  def sum(a: List[Double]): Double
  def concat(a: Seq[String]): String
  def max(a: List[Int]): Int // gives Int.MinValue if a is empty

trait Combiner[A]:
  def unit: A
  def combine(a: A, b: A): A

object FunctionsImpl extends Functions:
  given Combiner[Double] with
    override def unit = 0
    override def combine(a: Double, b: Double): Double = a + b

  given Combiner[String] with
    override def unit = ""
    override def combine(a: String, b: String): String = a + b

  given Combiner[Int] with
    override def unit = Int.MinValue
    override def combine(a: Int, b: Int): Int = if a > b then a else b

  def combine[A: Combiner](a: Seq[A])(using com: Combiner[A]): A = a.foldRight(com.unit)(com.combine)

  override def sum(a: List[Double]): Double = combine(a)
  override def concat(a: Seq[String]): String = combine(a)
  override def max(a: List[Int]): Int = combine(a)