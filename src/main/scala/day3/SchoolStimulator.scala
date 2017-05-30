package day3
import scala.collection.mutable

/**
  * Created by Pramod on 5/30/2017.
  */

abstract class SchoolEvent
case class AddCourseToSchool(school: School, course: Course) extends SchoolEvent
case class RemoveCourseFromSchool(school: School, course: Course) extends SchoolEvent

case class School(name: String){
  println(s"A school $name has been created")
  var coursesOffered: mutable.ListBuffer[Course] = new mutable.ListBuffer[Course]()

  def schoolActions(event: SchoolEvent) = {
    event match {
      case AddCourseToSchool(school, course) => {
        println(s"Trying to add course ${course.name} to school: ${school.name}")
        if(! school.coursesOffered.contains(course)){
          school.coursesOffered += course
          println("Added the course to the School")
        }else{
          println(s"Course ${course.name} already exists in ${school.name}")
        }
      }
      case RemoveCourseFromSchool(school, course) => {
        println(s"Trying to remove course ${course.name} from school: ${school.name}")
        if(school.coursesOffered.contains(course)){
          school.coursesOffered -= course
          println(s"Removed the course ${course.name} from the School: ${school.name}")
        }else{
          println(s"Course ${course.name} does not exist in ${school.name}")
        }
      }
      case _ => println("No action performed")
    }
  }
}

abstract class StudentEvent
case class EnrollStudentIntoCourse(course: Course) extends StudentEvent
case class RemoveStudentFromCourse(course: Course) extends StudentEvent

case class Student(id: Long, name: String, var school: School){
  println(s"A student named $name with id $id has been created ")
  var enrolledCourses: mutable.ListBuffer[Course] = new mutable.ListBuffer[Course]()

  def studentActions(event: StudentEvent) = {
    event match {
      case EnrollStudentIntoCourse(course) => {
        println(s"Trying to enrol student: ${this.name} in course ${course.name}")
        if(! enrolledCourses.contains(course)){
          this.enrolledCourses += course
          course.enrolledStudents += this
          println(s"Enrolled ${this.name} in the course:${course.name}")
        }else {
          println(s"${this.name} is not enrolled in the course:${course.name}")
        }
      }
      case RemoveStudentFromCourse(course) => {
        println(s"Trying to remove student: ${this.name} from course ${course.name}")
        if(enrolledCourses.contains(course)){
          this.enrolledCourses -= course
          course.enrolledStudents -= this
          println(s"Removed ${this.name} from the course:${course.name}")
        }else{
          println(s"${this.name} is not enrolled in the course:${course.name}")
        }
      }
      case _ => println("No action performed")
    }
  }
}

object Student{
  def apply(id: Long, name: String) = new Student(id, name, null)
}

case class Course(school: School, name: String){
  println(s"A course: $name has been created for the school $school")
  var enrolledStudents: mutable.ListBuffer[Student] = new mutable.ListBuffer[Student]()
}

object SchoolStimulator {

  def main(args: Array[String]): Unit = {
    var school: School = School("Indiana University")

    var student1: Student = Student(100, "Pramod", school)
    var student2: Student = Student(101, "Krishna", school)

    var course1: Course = Course(school, "Algorithms")
    var course2: Course = Course(school, "DataStructures")

    school.schoolActions(AddCourseToSchool(school, course1))
    school.schoolActions(RemoveCourseFromSchool(school, course2))
    school.schoolActions(AddCourseToSchool(school, course2))

    student1.studentActions(EnrollStudentIntoCourse(course1))
    student1.studentActions(RemoveStudentFromCourse(course2))
    student1.studentActions(EnrollStudentIntoCourse(course2))
  }
}
