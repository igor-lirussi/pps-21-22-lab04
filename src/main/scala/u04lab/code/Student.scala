package u04lab.code


trait Student:
  def name: String
  def year: Int
  def enrolling(course: Course* ): Unit // the student participates to a Course
  def courses: List[String] // names of course the student participates to
  def hasTeacher(teacher: String): Boolean // is the student participating to a course of this teacher?

trait Course:
  def name: String
  def teacher: String

object Student:
  def apply(name: String, year: Int = 2017): Student = StudentImpl(name, year)
  private case class StudentImpl(override val name:String, override val year:Int) extends Student:
    import u04lab.code.List.*
    private var coursesList :List[Course] = Nil()
    override def courses: List[String] = List.map(coursesList)(c => c.name)
    override def enrolling(course: Course*): Unit =   course foreach ( course => coursesList = Cons(course, coursesList) )
    override def hasTeacher(teacher: String): Boolean = List.contains(List.map(coursesList)( c=>c.teacher),teacher)

object Course:
  def apply(name: String, teacher: String): Course = CourseImpl(name, teacher)
  private case class CourseImpl(override val name:String, override val teacher:String) extends Course

object sameTeacher:
  import u04lab.code.List.*
  def unapply(courses: List[Course]): scala.Option[String] =
    courses match
      case Nil() => scala.Option.empty
      case Cons(course,t) => List.foldLeft(t)(scala.Option(course.teacher))((teacher, c) => teacher.filter(_ == c.teacher))


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

  //list factory (apply) gets now variable arguments
  val courses = List(cPPS, cPPS, cPPS)
  //extractor sameTeacher
  courses match
    case sameTeacher(t) => println (s" $courses have same teacher $t") // sameTeacher.unapply(courses) -> Option(teacher:String)
    case _ => println (s" $courses have different teachers ")

/** Hints:
  *   - simply implement Course, e.g. with a case class
  *   - implement Student with a StudentImpl keeping a private Set of courses
  *   - try to implement in StudentImpl method courses with map
  *   - try to implement in StudentImpl method hasTeacher with map and find
  *   - check that the two println above work correctly
  *   - refactor the code so that method enrolling accepts a variable argument Course*
  */
