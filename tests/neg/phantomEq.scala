/* This is a example of how to implement Eq using erasable phantom types.
 *
 * See also: ../pos/phantomEq.scala
 */


object PhantomEq {
  import PhantomEqUtil._
  import EqUtil._

  "abc" === "abc"
  1 === 4

  1 === "abc" // error
  "ghi" === 4 // error
  0 === Nil // error
  List(1, 2) === 1 // error
  List(1, 2) === "" // error

}
object EqUtil {
  import PhantomEqUtil._
  implicit class EqualsDeco[T](val x: T) extends AnyVal {
    def ===[U] (y: U)(implicit ce: PhantomEq[T, U]) = x.equals(y)
  }
}

object PhantomEqUtil extends PhantomAny {
  class PhantomEq[-L, -R] extends PhantomAny
  type PhantomEqEq[T] = PhantomEq[T, T]

  implicit def eqString: PhantomEqEq[String] = new PhantomEqEq[String]
  implicit def eqInt: PhantomEqEq[Int] = new PhantomEqEq[Int]
  implicit def eqDouble: PhantomEqEq[Double] = new PhantomEqEq[Double]

  implicit def eqByteNum: PhantomEq[Byte, Number] = new PhantomEq[Byte, Number]
  implicit def eqNumByte: PhantomEq[Number, Byte] = new PhantomEq[Number, Byte]

  implicit def eqSeq[T, U](implicit eq: PhantomEq[T, U]): PhantomEq[Seq[T], Seq[U]] = new PhantomEq[Seq[T], Seq[U]]
}
