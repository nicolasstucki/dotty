import scala.util.control.Breaks
import scala.util.control.ControlThrowable
import scala.util.control.NoStackTrace

object Test {
  def main(args: Array[String]): Unit = {
    System.out.println("============")
    System.out.println("hello")

//    barPoly("a", "b")
//    fooPoly(1)
//    fooInt(1, 2)
//    fooAnyRef("a")
//    List()
//    List(1)
//    List("1")

    new Breaks

    System.out.println("bye")
  }


//  def barPoly[S](xs: S*) = println("barPoly")
//  def fooPoly[T](x: T) = barPoly(x)
//  def fooInt(xs: Int*) = println("fooInt")
//  def fooAnyRef(xs: AnyRef*) = println("fooAnyRef")
}
