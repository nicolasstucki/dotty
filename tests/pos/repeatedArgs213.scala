import scala.collection.{immutable, mutable}
import java.nio.file.Paths

// Code below is to trick the compiler into thinking that we are
// compiling with the 2.13 standard library on the classpath.
package scala.collection {
  class IterableOnce
}

package scala.runtime {
  object ScalaRunTime {
    abstract class ArraySeq[+A] extends immutable.Seq[A]

    def genericWrapArray[T](xs: Array[T]): ArraySeq[T]          = ???
    def wrapRefArray[T <: AnyRef](xs: Array[T]): ArraySeq[T]    = ???
    def wrapIntArray(xs: Array[Int]): ArraySeq[Int]             = ???
    def wrapDoubleArray(xs: Array[Double]): ArraySeq[Double]    = ???
    def wrapLongArray(xs: Array[Long]): ArraySeq[Long]          = ???
    def wrapFloatArray(xs: Array[Float]): ArraySeq[Float]       = ???
    def wrapCharArray(xs: Array[Char]): ArraySeq[Char]          = ???
    def wrapByteArray(xs: Array[Byte]): ArraySeq[Byte]          = ???
    def wrapShortArray(xs: Array[Short]): ArraySeq[Short]       = ???
    def wrapBooleanArray(xs: Array[Boolean]): ArraySeq[Boolean] = ???
    def wrapUnitArray(xs: Array[Unit]): ArraySeq[Unit]          = ???
  }
}

// Start of Test
class repeatedArgs {
  def bar(xs: String*): Int = xs.length

  def test(xs: immutable.Seq[String]): Unit = {
    bar("a", "b", "c")
    bar(xs: _*)

    Paths.get("Hello", "World")
    Paths.get("Hello", xs: _*)

    val List(_, others: _*) = xs.toList // toList should not be needed, see #4790
    val x: immutable.Seq[String] = others
  }
}
