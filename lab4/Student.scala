package u04lab.code

import List.*

trait Student:
  def name: String
  def year: Int
  def enrolling(course: Course): Unit // the student participates to a Course
  def courses: List[String] // names of course the student participates to
  def hasTeacher(teacher: String): Boolean // is the student participating to a course of this teacher?

trait Course:
  def name: String
  def teacher: String

object Student:
  def apply(name: String, year: Int = 2017): Student = StudentImpl(name, year)
  private case class StudentImpl(override val name: String,
                                 override val year: Int) extends Student:
    private var l: List[Course] = Nil()
    override def enrolling(course: Course): Unit = l = append(Cons(course,Nil()), l)

    def enrollingList(courses: List[Course]): Unit = l = append(courses, l)
    def enrollingList2(courses: Course*): Unit = courses.foreach(c => enrolling(c))
    def enrollingList3(courses: Course*): Unit = for c <- courses do enrolling(c)

    override def courses: List[String] = map(l)( _.name)

    override def hasTeacher(teacher: String): Boolean = contains(map(l)(_.teacher), teacher)

object Course:
  def apply(name: String, teacher: String): Course = CourseImpl(name, teacher)
  private case class CourseImpl(override val name: String,
                               override val teacher: String) extends Course

@main def checkStudents(): Unit =
  val cPPS = Course("PPS", "Viroli")
  val cPCD = Course("PCD", "Ricci")
  val cSDR = Course("SDR", "D'Angelo")
  val s1 = Student("mario", 2015)
  val s2 = Student("gino", 2016)
  val s3 = Student("rino") // defaults to 2017
  s1.enrolling(cPPS)
  s1.enrolling(cPCD)
  s2.enrolling(cPPS)
  s3.enrolling(cPPS)
  s3.enrolling(cPCD)
  s3.enrolling(cSDR)
  println(
    (s1.courses, s2.courses, s3.courses)
  ) // (Cons(PCD,Cons(PPS,Nil())),Cons(PPS,Nil()),Cons(SDR,Cons(PCD,Cons(PPS,Nil()))))
  println(s1.hasTeacher("Ricci")) // true

/** Hints:
  *   - simply implement Course, e.g. with a case class
  *   - implement Student with a StudentImpl keeping a private Set of courses
  *   - try to implement in StudentImpl method courses with map
  *   - try to implement in StudentImpl method hasTeacher with map and find
  *   - check that the two println above work correctly
  *   - refactor the code so that method enrolling accepts a variable argument Course*
  */
